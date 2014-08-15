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


