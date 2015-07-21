%%% http_test_lib.erl
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
-module(http_test_lib).

-export([start_origin/2, start_proxy/1]).
-export([stop_origin/1, stop_proxy/1]).
-export([connect/1, receive_headers/1, origin_host/0]).

-include_lib("common_test/include/ct.hrl").

start_origin(Config, Options) ->
	PrivDir = ?config(priv_dir, Config),
	{ok, ServerName} = inet:gethostname(),
	OriginPort = ct:get_config({http_origin, port}, 8080),
	{ok, OriginPid} = inets:start(httpd,
			[{server_name, ServerName}, {port, OriginPort},
			{server_root, PrivDir}, {document_root, PrivDir},
			{mime_types, [{"html", "text/html"}, {"htm", "text/html"},
					{"txt", "text/plain"}]} | Options]),
	case lists:keyfind(modules, 1, Options) of
		{modules, Modules} ->
			ok = httpd_util:enable_debug([{exported_functions, Modules}]);
		false ->
			ok
	end,
	[{origin_pid, OriginPid} | Config].

start_proxy(Config) ->
	ProxyAddress = ct:get_config({http_proxy, address}, "localhost"),
	ProxyPort = ct:get_config({http_proxy, port}, 3128),
	{ok, ProxyPid} = http_proxy:start(ProxyPort, []),
	[{proxy_address, ProxyAddress}, {proxy_port, ProxyPort},
			{proxy_pid, ProxyPid} | Config].

stop_origin(Config) ->
	case ?config(origin_pid, Config) of
		undefined ->
			ok;
		OriginPid ->
			ok = inets:stop(httpd, OriginPid),
			lists:keydelete(origin_pid, 1, Config)
	end.

stop_proxy(Config) ->
	case ?config(proxy_pid, Config) of
		undefined ->
			ok;
		ProxyPid ->
			ok = http_proxy:stop(ProxyPid),
			lists:keydelete(proxy_pid, 1, Config)
	end.

origin_host() ->
	Hostname = case ct:get_config({http_origin, address}) of
		undefined ->
			{ok, Name} = inet:gethostname(),
			Name;
		Address when is_tuple(Address) ->
			inet:ntoa(Address);
		Address ->
			Address
	end,
	case ct:get_config({http_origin, port}, 8080) of
		80 ->
			Hostname;
		OriginPort ->
			Hostname ++ ":" ++ integer_to_list(OriginPort)
	end.

connect(Config) ->
	ProxyAddress = ?config(proxy_address, Config),
	ProxyPort = ?config(proxy_port, Config),
	{ok, Socket} = gen_tcp:connect(ProxyAddress, ProxyPort,
			[{packet, http}, {active, false}]),
	Socket.

receive_headers(Socket) ->
	receive_headers(Socket, []).
receive_headers(Socket, Acc) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, http_eoh} ->
			Acc;
		{ok, {http_header, _, HttpField, _, Value}} ->
			receive_headers(Socket, [{HttpField, Value} | Acc])
	end.

