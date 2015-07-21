%%% http_transfer.erl
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
-module(http_transfer).

-export([te/3]).

-define(CHUNKSIZE, 512).

te(SessionID, Env, Input)-> 
	Headers = "Content-Type: application/octet-stream\r\n\r\n",
	Trailers = [],
	case mod_esi:deliver(SessionID, Headers) of
		ok ->
			{_, Content, _} = code:get_object_code(list_to_atom(Input)),
%			case lists:keyfind('TE', Env) of
%				{'TE', "chunked"} ->
					chunk(SessionID, Trailers, Content);
%				{'TE', "deflate"} ->
%					deflate(SessionID, Trailers, Content);
%			end;
		{error, Reason} ->
			{error, Reason}
	end.
	
chunk(SessionID, Trailers, <<Chunk:?CHUNKSIZE/bytes, Rest/bytes>>) ->
	case mod_esi:deliver(SessionID, Chunk) of
		ok ->
			chunk(SessionID, Trailers, Rest);
		{error, Reason} ->
			{error, Reason}
	end;
chunk(SessionID, Trailers, Rest) ->
	case mod_esi:deliver(SessionID, Rest) of
		ok ->
			mod_esi:deliver(SessionID, Trailers);
		{error, Reason} ->
			{error, Reason}
	end.

deflate(SessionID, Trailers, <<Chunk:?CHUNKSIZE/bytes, Rest/bytes>>) ->
	% @todo support deflate encoding
	case mod_esi:deliver(SessionID, Chunk) of
		ok ->
			deflate(SessionID, Trailers, Rest);
		{error, Reason} ->
			{error, Reason}
	end;
deflate(SessionID, Trailers, Rest) ->
	case mod_esi:deliver(SessionID, Rest) of
		ok ->
			mod_esi:deliver(SessionID, Trailers);
		{error, Reason} ->
			{error, Reason}
	end.


