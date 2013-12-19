-module(http_test_lib).

-export([start_origin/2, start_proxy/1]).
-export([stop_origin/1, stop_proxy/1]).
-export([connect/0, receive_headers/1, origin_host/0]).

-include_lib("common_test/include/ct.hrl").

start_origin(Config, Options) ->
	PrivDir = ?config(priv_dir, Config),
	{ok, ServerName} = inet:gethostname(),
	OriginPort = ct:get_config({http_origin, port}, 8080),
	{ok, OriginPid} = inets:start(httpd,
			[{server_name, ServerName}, {port, OriginPort},
			{server_root, PrivDir}, {document_root, PrivDir} | Options]),
	%case lists:keyfind(modules, 1, Options) of
	%	{modules, Modules} ->
	%		ok = httpd_util:enable_debug([{exported_functions, Modules}]);
	%	false ->
	%		ok
	%end,
	[{origin_pid, OriginPid} | Config].

start_proxy(Config) ->
	ProxyPort = ct:get_config({http_proxy, port}, 3128),
	{ok, ProxyPid} = http_proxy:start(ProxyPort, []),
	[{proxy_pid, ProxyPid} | Config].

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

connect() ->
	ProxyAddress = ct:get_config({http_proxy, address}, "localhost"),
	ProxyPort = ct:get_config({http_proxy, port}, 3128),
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

