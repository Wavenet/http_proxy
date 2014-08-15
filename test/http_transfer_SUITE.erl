-module(http_transfer_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
	[{userdata, [{info, "This suite tests HTTP transfer encoding"}]},
			% {require, {http_proxy, [port]}},
			{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
	ModuleList = code:all_loaded(),
	random:seed(os:timestamp()),
	N = random:uniform(length(ModuleList)),
	{Module, _} = lists:nth(N, ModuleList),
	{_, Content, _} = code:get_object_code(Module),
	Config1 = [{content_path, "/cgi-bin/http_transfer/te?" ++ atom_to_list(Module)},
			{entity_content, Content} | Config],
	ok = inets:start(),
	Config2 = http_test_lib:start_origin(Config1, [{modules, [mod_esi]},
			{erl_script_alias, {"/cgi-bin", [http_transfer]}}]),
	ok = application:start(http_proxy),
	Config3 = http_test_lib:start_proxy(Config2),
	ProxyAddress = ?config(proxy_address, Config3),
	ProxyPort = ?config(proxy_port, Config3),
	ok = httpc:set_options([{proxy, {{ProxyAddress, ProxyPort}, []}}]),
	Config3.

end_per_suite(Config) ->
	Config1 = http_test_lib:stop_proxy(Config),
	ok = application:stop(http_proxy),
	Config2 = http_test_lib:stop_origin(Config1),
	ok = inets:stop(),
	Config2.

all() ->
	[chunked].

chunked() ->
	[{userdata, [{doc, "Test the chunked transfer encoding"}]}].

chunked(Config) ->
	Path = ?config(content_path, Config),
	OriginHost = http_test_lib:origin_host(),
	URI = "http://" ++ OriginHost ++ Path,
	Content = ?config(entity_content, Config),
	ContentLength = integer_to_list(size(Content)),
	RequestHeaders = [{"TE", "chunked"}],
	{ok, {{_,200,_}, ResponseHeaders, _Content}} = httpc:request(get,
			{URI, RequestHeaders}, [], [{body_format, binary}]),
	{_, ContentLength} = lists:keyfind("content-length", 1, ResponseHeaders),
	{_, "application/octet-stream"} = lists:keyfind("content-type", 1, ResponseHeaders).

