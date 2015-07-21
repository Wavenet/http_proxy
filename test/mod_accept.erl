%%% mod_accept.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author Vance Shipley <vances@globalwavenet.com>
%%% @copyright 2013-2015 Global Wavenet (Pty) Ltd
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%% 
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(mod_accept).

-export([do/1]).

-include_lib("inets/include/httpd.hrl"). 

%% TODO: support status code 406 Not Acceptable (instead of 404)

do(#mod{request_uri = URI, parsed_header = RequestHeaders,
		config_db = ConfigDB, data = Data}) ->
	case lists:keyfind("accept", 1, RequestHeaders) of
      {"accept", Accept} ->
			{Path, AfterPath} = httpd_util:split_path(URI),
			case catch parse_accept(Path, ConfigDB, Accept) of
				{'EXIT', Reason} ->
					{proceed, [{status, {400, none, Reason}} | Data]};
				{ok, NewPath} ->
					{proceed, [{real_name, {NewPath, AfterPath}} | Data]};
				none ->
					{proceed, Data}
			end;
      false ->
			{proceed, Data}
   end.

parse_accept(Path, ConfigDB, Accept) ->
	DocumentRoot = httpd_util:lookup(ConfigDB, document_root),
	BasePath = DocumentRoot ++ Path,
	MediaRanges = parse_media_range(string:tokens(Accept, ","), []),
	SuffixMap = map_suffixes(ConfigDB),
	real_name(BasePath, SuffixMap, MediaRanges).
	
parse_media_range([], Acc) ->
	reorder(lists:keysort(1, Acc), []);
parse_media_range([MediaRange | T], Acc) ->
	% todo: handle accept-extension 
	[{q, N} | _Extensions] = case string:tokens(MediaRange, ";") of
		[MediaRange] ->
			[{q, 1000}];
		[MediaRange, Params] ->
			parse_accept_params(Params)
	end,
	parse_media_range(T, [{N, MediaRange} | Acc]).

parse_accept_params([H | T]) ->
	% RFC2616 3.9 Quality Values
	N = case string:tokens(H, [$ , $\t, $\n, $\r, $=]) of
		["q", [$0, $., N1]] ->
			N1 * 100;
		["q", [$0, $., N1, N2]] ->
			(N1 * 100) + (N2 * 10);
		["q", [$0, $., N1, N2, N3]] ->
			(N1 * 100) + (N2 * 10) + N3
	end,
	parse_accept_params(T, [{q, N}]).

parse_accept_params([], Acc) ->
	lists:reverse(Acc);
parse_accept_params([H | T], Acc) ->
	[Param, Value] = string:tokens(H, [$ , $\t, $\n, $\r, $=]),
	parse_accept_params(T, [{Param, Value} | Acc]).

reorder([{N, MediaRange} | T], Acc) ->
	reorder(T, N, [string:tokens(MediaRange, "/")], Acc);
reorder([], [Acc]) ->
	Acc.

reorder([{N, MediaRange} | T], N, Acc1, Acc2) ->
	reorder(T, N, [string:tokens(MediaRange, "/") | Acc1], Acc2);
reorder(MediaRanges, _, Acc1, Acc2) ->
	F = fun(["*", "*"], [Type, "*"]) when Type /= "*" ->
				false;
			([Type, "*"], [Type, Subtype]) when Type /= "*", Subtype /= "*" ->
				false;
			(_, _) ->
				true
	end,
	reorder(MediaRanges, [lists:sort(F, Acc1) | Acc2]).

real_name(BasePath, SuffixMap, [[Type, "*"] | T]) ->
	real_name(BasePath, SuffixMap, Type, T, gb_trees:iterator(SuffixMap));
real_name(BasePath, SuffixMap, [[Type, SubType] | T]) ->
	case gb_trees:lookup(Type ++ "/" ++ SubType, SuffixMap) of
		{value, Suffixes} ->
			case file_info(BasePath, Suffixes) of
				{ok, NewPath} ->
					{ok, NewPath};
				none ->
					real_name(BasePath, SuffixMap, T)
			end;
		none ->
			real_name(BasePath, SuffixMap, T)
	end;
real_name(_, _, []) ->
	none.

real_name(BasePath, SuffixMap, "*", T, Iter) ->
	case gb_trees:next(Iter) of
		{_, Suffixes, NewIter} ->
			case file_info(BasePath, Suffixes) of
				{ok, NewPath} ->
					{ok, NewPath};
				none ->
					real_name(BasePath, SuffixMap, "*", T, NewIter)
			end;
		none ->
			real_name(BasePath, SuffixMap, T)
	end;
real_name(BasePath, SuffixMap, Type, T, Iter) ->
	case gb_trees:next(Iter) of
		{MimeType, Suffixes, NewIter} ->
			case lists:prefix(Type ++ "/", MimeType) of
				true ->
					case file_info(BasePath, Suffixes) of
						{ok, NewPath} ->
							{ok, NewPath};
						none ->
							real_name(BasePath, SuffixMap, Type, T, NewIter)
					end;
				false ->
					real_name(BasePath, SuffixMap, Type, T, NewIter)
			end;
		none ->
			real_name(BasePath, SuffixMap, T)
	end.

file_info(_, []) ->
	none;
file_info(BasePath, [H | T]) ->
	NewPath = BasePath ++ "." ++ H,
	case file:read_file_info(NewPath) of
		{ok, _FileInfo} ->
			{ok, NewPath};
		{error, _Reason} ->
			file_info(BasePath, T)
	end.

-spec map_suffixes(ConfigDB :: ets:tab()) -> gb_trees:gb_tree().
% build a reverse lookup tree
map_suffixes(ConfigDB) ->
	[{mime_types, MimeTypesDB} | _] = ets:lookup(ConfigDB, mime_types),
	Ftypes = fun(Type, {Suffix, Acc}) ->
				case gb_trees:lookup(Type, Acc) of
					none ->
						{Suffix, gb_trees:insert(Type, [Suffix], Acc)};
					{value, Suffixes} ->
						{Suffix, gb_trees:enter(Type, [Suffix | Suffixes], Acc)}
				end
	end,
	Fsuffixes = fun({Suffix, MimeTypes}, Acc) ->
				L = string:tokens(MimeTypes, ","),
				{_, NewAcc} = lists:foldl(Ftypes, {Suffix, Acc}, L),
				NewAcc
	end,
	ets:foldl(Fsuffixes, gb_trees:empty(), MimeTypesDB).

