%%% http_options_SUITE.erl
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
-module(http_options_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
	[{userdata, [{info, "This suite tests the HTTP OPTIONS method."}]},
			% {require, {http_proxy, [port]}},
			{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
	PrivDir = ?config(priv_dir, Config),
	Reference = base64:encode_to_string(erlang:ref_to_list(make_ref())),
	FileName = Reference ++ ".html",
	Path = PrivDir ++ "/" ++ FileName,
	Content = ["<html><head><title>", atom_to_list(?MODULE),
			"</title></head><body>", Reference, "</body></html>"],
	ok = file:write_file(Path, Content),
	Config1 = [{html_filename, FileName}, {html_path, Path},
			{html_content, lists:flatten(Content)} | Config],
	ok = inets:start(),
	Config2 = http_test_lib:start_origin(Config1, []),
	ok = application:start(http_proxy),
	http_test_lib:start_proxy(Config2).

end_per_suite(Config) ->
	Config1 = http_test_lib:stop_proxy(Config),
	ok = application:stop(http_proxy),
	Config2 = http_test_lib:stop_origin(Config1),
	ok = inets:stop(),
	Config2.

all() ->
	[options_wildcard, options_no_forward, options_origin].

options_wildcard() ->
	[{userdata, [{doc, "Test the OPTIONS method with a wildcard."}]}].

options_wildcard(Config) ->
	Socket = http_test_lib:connect(Config),
	OriginHost = http_test_lib:origin_host(),
	ok = gen_tcp:send(Socket, ["OPTIONS * HTTP/1.1", [13, 10],
			"Host: ", OriginHost, [13, 10, 13, 10]]),
	{ok, {http_response, _, 204, _}} = gen_tcp:recv(Socket, 0),
	ResponseHeaders = http_test_lib:receive_headers(Socket),
	{_, _Allow} = lists:keyfind('Allow', 1, ResponseHeaders).

options_no_forward() ->
	[{userdata, [{doc, "Test the OPTIONS method with zero forwards."}]}].

options_no_forward(Config) ->
	FileName = ?config(html_filename, Config),
	Socket = http_test_lib:connect(Config),
	OriginHost = http_test_lib:origin_host(),
	ok = gen_tcp:send(Socket, ["OPTIONS /", FileName, " HTTP/1.1", [13, 10],
			"Host: ", OriginHost, [13, 10],
			"Max-Forwards: 0", [13, 10, 13, 10]]),
	{ok, {http_response, _, 204, _}} = gen_tcp:recv(Socket, 0),
	ResponseHeaders = http_test_lib:receive_headers(Socket),
	{_, _Allow} = lists:keyfind('Allow', 1, ResponseHeaders).

options_origin() ->
	[{userdata, [{doc, "Test the OPTIONS method with one forward."}]}].

options_origin(Config) ->
	FileName = ?config(html_filename, Config),
	Socket = http_test_lib:connect(Config),
	OriginHost = http_test_lib:origin_host(),
	ok = gen_tcp:send(Socket, ["OPTIONS /", FileName, " HTTP/1.1", [13, 10],
			"Host: ", OriginHost, [13, 10],
			"Max-Forwards: 1", [13, 10, 13, 10]]),
	{ok, {http_response, _, 204, _}} = gen_tcp:recv(Socket, 0),
	ResponseHeaders = http_test_lib:receive_headers(Socket),
	{_, _Allow} = lists:keyfind('Allow', 1, ResponseHeaders).

