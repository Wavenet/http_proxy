%%% http_proxy_util.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  @author Vance Shipley <vance@wavenet.lk>
%%%  @copyright 2013 Wavenet International (Pvt) Ltd.
%%%  @end
%%%  This computer program(s) is proprietary software and the intellectual
%%%  property of WAVENET INTERNATIONAL (PVT) LIMITED (hereinafter referred
%%%  to as "Wavenet").  Unless otherwise specified, all materials contained
%%%  in herein are copyrighted and may not be used except as provided in 
%%%  these terms and conditions or in the copyright notice (documents and
%%%  software) or other proprietary notice provided with, or attached to,
%%%  the software or relevant document, or is otherwise referenced as 
%%%  applicable to the software.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements utilities for the
%%% 	{@link //http_proxy. http_proxy} application.
%%%
-module(http_proxy_util).
-copyright('Copyright (c) 2013 Wavenet International (Pvt) Ltd.').
-author('Vance Shipley <vance@wavenet.lk>').

-include("http_proxy.hrl").

%% export the http_proxy_util public API
-export([encode_request/4, encode_response/4, encode_response/5,
		encode_head/1]).

%%----------------------------------------------------------------------
%%  The http_proxy_util API
%%----------------------------------------------------------------------

-spec encode_request(Method :: http_method(), URI :: binary(),
		Head :: [http_field()], Body :: binary() | undefined) -> iodata().
%% @doc Encode an HTTP request from data received through
%% 	{@link //erts/erlang:decode_packet/3. erlang:decode_packet/3}.
encode_request(Method, URI, Head, undefined = _Body)
		when is_atom(Method), is_binary(URI), is_list(Head) ->
	[atom_to_list(Method), 32, URI, 32, <<"HTTP/1.1">>, 13, 10,
		encode_head(Head), 13, 10];
encode_request(Method, URI, Head, Body)
		when is_atom(Method), is_binary(URI), is_list(Head), is_binary(Body) ->
	[atom_to_list(Method), 32, URI, 32, <<"HTTP/1.1">>, 13, 10,
		encode_head(Head), 13, 10, Body].

-spec encode_response(Version :: http_version(), StatusCode :: pos_integer(),
		Head :: [http_field()], Body :: binary() | undefined) -> iodata().
%% @doc Encode an HTTP request from data received through
%% 	{@link //erts/erlang:decode_packet/3. erlang:decode_packet/3}.
%% @todo Should version of response match version of request?
encode_response({1, 1}, StatusCode, Head, Body)
		when is_integer(StatusCode), is_list(Head) ->
	StatusString =  httpd_util:reason_phrase(StatusCode),
	encode_response({1, 1}, StatusCode, StatusString, Head, Body).
	
-spec encode_response(Version :: http_version(), StatusCode :: pos_integer(),
		StatusString :: binary() | string(), Head :: [http_field()],
		Body :: binary() | undefined) -> iodata().
%% @doc Encode an HTTP request from data received through
%% 	{@link //erts/erlang:decode_packet/3. erlang:decode_packet/3}.
%% @todo Should version of response match version of request?
encode_response({1, 1}, StatusCode, StatusString, Head, undefined = _Body)
		when is_integer(StatusCode), is_list(Head) ->
	[<<"HTTP/1.1 ">>, integer_to_binary(StatusCode), 32,
			StatusString, 13, 10, encode_head(Head), 13, 10];
encode_response({1, 1}, StatusCode, StatusString, Head, Body)
		when is_integer(StatusCode), is_list(Head) ->
	[<<"HTTP/1.1 ">>, integer_to_binary(StatusCode), 32,
			StatusString, 13, 10, encode_head(Head), 13, 10, Body].

-spec encode_head(Head :: [http_field()]) -> iodata().
%% @doc Encodes an HTTP head from data received through
%% 	{@link //erts/erlang:decode_packet/3. erlang:decode_packet/3}.
encode_head(Head) ->
	encode_head(Head, []).
%% @hidden
encode_head([{'Host', Host} | T], Acc) ->
	encode_head(T, [[<<"Host: ">>, Host, 13, 10] | Acc]);
encode_head([{'Cache-Control', Value} | T], Acc) ->
	encode_head(T, [[<<"Cache-Control: ">>, Value, 13, 10] | Acc]);
encode_head([{'Connection', Value} | T], Acc) ->
	encode_head(T, [[<<"Connection: ">>, Value, 13, 10] | Acc]);
encode_head([{'Date', Value} | T], Acc) ->
	encode_head(T, [[<<"Date: ">>, Value, 13, 10] | Acc]);
encode_head([{'Pragma', Value} | T], Acc) ->
	encode_head(T, [[<<"Pragma: ">>, Value, 13, 10] | Acc]);
encode_head([{'Transfer-Encoding', Value} | T], Acc) ->
	encode_head(T, [[<<"Transfer-Encoding: ">>, Value, 13, 10] | Acc]);
encode_head([{'Upgrade', Value} | T], Acc) ->
	encode_head(T, [[<<"Upgrade: ">>, Value, 13, 10] | Acc]);
encode_head([{'Via', Value} | T], Acc) ->
	encode_head(T, [[<<"Via: ">>, Value, 13, 10] | Acc]);
encode_head([{'Accept', Value} | T], Acc) ->
	encode_head(T, [[<<"Accept: ">>, Value, 13, 10] | Acc]);
encode_head([{'Accept-Charset', Value} | T], Acc) ->
	encode_head(T, [[<<"Accept-Charset: ">>, Value, 13, 10] | Acc]);
encode_head([{'Accept-Encoding', Value} | T], Acc) ->
	encode_head(T, [[<<"Accept-Encoding: ">>, Value, 13, 10] | Acc]);
encode_head([{'Accept-Language', Value} | T], Acc) ->
	encode_head(T, [[<<"Accept-Language: ">>, Value, 13, 10] | Acc]);
encode_head([{'Authorization', Value} | T], Acc) ->
	encode_head(T, [[<<"Authorization: ">>, Value, 13, 10] | Acc]);
encode_head([{'From', Value} | T], Acc) ->
	encode_head(T, [[<<"From: ">>, Value, 13, 10] | Acc]);
encode_head([{'If-Modified-Since', Value} | T], Acc) ->
	encode_head(T, [[<<"If-Modified-Since: ">>, Value, 13, 10] | Acc]);
encode_head([{'If-Match', Value} | T], Acc) ->
	encode_head(T, [[<<"If-Match: ">>, Value, 13, 10] | Acc]);
encode_head([{'If-None-Match', Value} | T], Acc) ->
	encode_head(T, [[<<"If-None-Match: ">>, Value, 13, 10] | Acc]);
encode_head([{'If-Range', Value} | T], Acc) ->
	encode_head(T, [[<<"If-Range: ">>, Value, 13, 10] | Acc]);
encode_head([{'If-Unmodified-Since', Value} | T], Acc) ->
	encode_head(T, [[<<"If-Unmodified-Since: ">>, Value, 13, 10] | Acc]);
encode_head([{'Max-Forwards', Value} | T], Acc) ->
	encode_head(T, [[<<"Max-Forwards: ">>, Value, 13, 10] | Acc]);
encode_head([{'Proxy-Authorization', Value} | T], Acc) ->
	encode_head(T, [[<<"Proxy-Authorization: ">>, Value, 13, 10] | Acc]);
encode_head([{'Range', Value} | T], Acc) ->
	encode_head(T, [[<<"Range: ">>, Value, 13, 10] | Acc]);
encode_head([{'Referer', Value} | T], Acc) ->
	encode_head(T, [[<<"Referer: ">>, Value, 13, 10] | Acc]);
encode_head([{'User-Agent', Value} | T], Acc) ->
	encode_head(T, [[<<"User-Agent: ">>, Value, 13, 10] | Acc]);
encode_head([{'Age', Value} | T], Acc) ->
	encode_head(T, [[<<"Age: ">>, Value, 13, 10] | Acc]);
encode_head([{'Location', Value} | T], Acc) ->
	encode_head(T, [[<<"Location: ">>, Value, 13, 10] | Acc]);
encode_head([{'Proxy-Authenticate', Value} | T], Acc) ->
	encode_head(T, [[<<"Proxy-Authenticate: ">>, Value, 13, 10] | Acc]);
encode_head([{'Public', Value} | T], Acc) ->
	encode_head(T, [[<<"Public: ">>, Value, 13, 10] | Acc]);
encode_head([{'Retry-After', Value} | T], Acc) ->
	encode_head(T, [[<<"Retry-After: ">>, Value, 13, 10] | Acc]);
encode_head([{'Server', Value} | T], Acc) ->
	encode_head(T, [[<<"Server: ">>, Value, 13, 10] | Acc]);
encode_head([{'Vary', Value} | T], Acc) ->
	encode_head(T, [[<<"Vary: ">>, Value, 13, 10] | Acc]);
encode_head([{'Warning', Value} | T], Acc) ->
	encode_head(T, [[<<"Warning: ">>, Value, 13, 10] | Acc]);
encode_head([{'Www-Authenticate', Value} | T], Acc) ->
	encode_head(T, [[<<"Www-Authenticate: ">>, Value, 13, 10] | Acc]);
encode_head([{'Allow', Value} | T], Acc) ->
	encode_head(T, [[<<"Allow: ">>, Value, 13, 10] | Acc]);
encode_head([{'Content-Base', Value} | T], Acc) ->
	encode_head(T, [[<<"Content-Base: ">>, Value, 13, 10] | Acc]);
encode_head([{'Content-Encoding', Value} | T], Acc) ->
	encode_head(T, [[<<"Content-Encoding: ">>, Value, 13, 10] | Acc]);
encode_head([{'Content-Language', Value} | T], Acc) ->
	encode_head(T, [[<<"Content-Language: ">>, Value, 13, 10] | Acc]);
encode_head([{'Content-Length', Value} | T], Acc) ->
	encode_head(T, [[<<"Content-Length: ">>, Value, 13, 10] | Acc]);
encode_head([{'Content-Location', Value} | T], Acc) ->
	encode_head(T, [[<<"Content-Location: ">>, Value, 13, 10] | Acc]);
encode_head([{'Content-Md5', Value} | T], Acc) ->
	encode_head(T, [[<<"Content-Md5: ">>, Value, 13, 10] | Acc]);
encode_head([{'Content-Range', Value} | T], Acc) ->
	encode_head(T, [[<<"Content-Range: ">>, Value, 13, 10] | Acc]);
encode_head([{'Content-Type', Value} | T], Acc) ->
	encode_head(T, [[<<"Content-Type: ">>, Value, 13, 10] | Acc]);
encode_head([{'Etag', Value} | T], Acc) ->
	encode_head(T, [[<<"Etag: ">>, Value, 13, 10] | Acc]);
encode_head([{'Expires', Value} | T], Acc) ->
	encode_head(T, [[<<"Expires: ">>, Value, 13, 10] | Acc]);
encode_head([{'Last-Modified', Value} | T], Acc) ->
	encode_head(T, [[<<"Last-Modified: ">>, Value, 13, 10] | Acc]);
encode_head([{'Accept-Ranges', Value} | T], Acc) ->
	encode_head(T, [[<<"Accept-Ranges: ">>, Value, 13, 10] | Acc]);
encode_head([{'Set-Cookie', Value} | T], Acc) ->
	encode_head(T, [[<<"Set-Cookie: ">>, Value, 13, 10] | Acc]);
encode_head([{'Set-Cookie2', Value} | T], Acc) ->
	encode_head(T, [[<<"Set-Cookie2: ">>, Value, 13, 10] | Acc]);
encode_head([{'X-Forwarded-For', Value} | T], Acc) ->
	encode_head(T, [[<<"X-Forwarded-For: ">>, Value, 13, 10] | Acc]);
encode_head([{'Cookie', Value} | T], Acc) ->
	encode_head(T, [[<<"Cookie: ">>, Value, 13, 10] | Acc]);
encode_head([{'Keep-Alive', Value} | T], Acc) ->
	encode_head(T, [[<<"Keep-Alive: ">>, Value, 13, 10] | Acc]);
encode_head([{'Proxy-Connection', Value} | T], Acc) ->
	encode_head(T, [[<<"Proxy-Connection: ">>, Value, 13, 10] | Acc]);
encode_head([{Field, Value} | T], Acc)
		when is_binary(Field), is_binary(Value) ->
	encode_head(T, [[Field, $:, Value, 13, 10] | Acc]);
encode_head([], Acc) ->
	lists:reverse(Acc).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

