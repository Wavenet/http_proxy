-module(http_head_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
	[{userdata, [{info, "This suite tests the HTTP HEAD method"}]},
			% {require, {http_proxy, [port]}},
			{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
	PrivDir = ?config(priv_dir, Config),
	Reference = base64:encode_to_string(erlang:ref_to_list(make_ref())),
	Path = PrivDir ++ "/" ++ Reference,
	HtmlContent = ["<html><head><title>", atom_to_list(?MODULE),
			"</title></head><body>", Reference, "</body></html>"],
	ok = file:write_file(Path ++ ".html", HtmlContent),
	PlainTextContent = ["Title: ", atom_to_list(?MODULE), [$\n],
			"Reference: ", Reference],
	ok = file:write_file(Path ++ ".txt", PlainTextContent),
	Config1 = [{content_uri, "/" ++ Reference}, {content_path, Path},
			{html_uri, "/" ++ Reference ++ ".html"},
			{html_content, lists:flatten(HtmlContent)},
			{text_uri, "/" ++ Reference ++ ".txt"},
			{text_content, lists:flatten(PlainTextContent)} | Config],
	ok = inets:start(),
	Config2 = http_test_lib:start_origin(Config1, [{modules, [mod_accept, mod_head]}]),
	ok = application:start(http_proxy),
	http_test_lib:start_proxy(Config2).

end_per_suite(Config) ->
	Config1 = http_test_lib:stop_proxy(Config),
	ok = application:stop(http_proxy),
	Config2 = http_test_lib:stop_origin(Config1),
	ok = inets:stop(),
	Config2.

all() ->
	[head, accept_html, accept_text, accept_unsupported, noexist, nodir].

head() ->
	[{userdata, [{doc, "Test the HEAD method in simplest form"}]}].

head(Config) ->
	URI = ?config(html_uri, Config),
	Content = ?config(html_content, Config),
	ContentLength = integer_to_list(length(Content)),
	Socket = http_test_lib:connect(),
	OriginHost = http_test_lib:origin_host(),
	ok = gen_tcp:send(Socket, ["HEAD ", URI, " HTTP/1.1", [13, 10],
			"Host: ", OriginHost, [13, 10], [13, 10, 13, 10]]),
	{ok, {http_response, _, 200, _}} = gen_tcp:recv(Socket, 0),
	ResponseHeaders = http_test_lib:receive_headers(Socket),
	{_, "text/html"} = lists:keyfind('Content-Type', 1, ResponseHeaders),
	{_, ContentLength} = lists:keyfind('Content-Length', 1, ResponseHeaders).

accept_html() ->
	[{userdata, [{doc, "Test the HEAD method with HTML accepted"}]}].

accept_html(Config) ->
	URI = ?config(content_uri, Config),
	Content = ?config(html_content, Config),
	ContentLength = integer_to_list(length(Content)),
	Socket = http_test_lib:connect(),
	OriginHost = http_test_lib:origin_host(),
	ok = gen_tcp:send(Socket, ["HEAD ", URI, " HTTP/1.1", [13, 10],
			"Host: ", OriginHost, [13, 10],
			"Accept: text/html", [13, 10, 13, 10]]),
	{ok, {http_response, _, 200, _}} = gen_tcp:recv(Socket, 0),
	ResponseHeaders = http_test_lib:receive_headers(Socket),
	{_, "text/html"} = lists:keyfind('Content-Type', 1, ResponseHeaders),
	{_, ContentLength} = lists:keyfind('Content-Length', 1, ResponseHeaders).

accept_text() ->
	[{userdata, [{doc, "Test the HEAD method with only plain text accepted"}]}].

accept_text(Config) ->
	URI = ?config(content_uri, Config),
	Content = ?config(text_content, Config),
	ContentLength = integer_to_list(length(Content)),
	Socket = http_test_lib:connect(),
	OriginHost = http_test_lib:origin_host(),
	ok = gen_tcp:send(Socket, ["HEAD ", URI, " HTTP/1.1", [13, 10],
			"Host: ", OriginHost, [13, 10],
			"Accept: text/plain", [13, 10, 13, 10]]),
	{ok, {http_response, _, 200, _}} = gen_tcp:recv(Socket, 0),
	ResponseHeaders = http_test_lib:receive_headers(Socket),
	{_, "text/plain"} = lists:keyfind('Content-Type', 1, ResponseHeaders),
	{_, ContentLength} = lists:keyfind('Content-Length', 1, ResponseHeaders).

accept_unsupported() ->
	[{userdata, [{doc, "Test the HEAD method with an unsupported content type"}]}].

accept_unsupported(Config) ->
	URI = ?config(content_uri, Config),
	Socket = http_test_lib:connect(),
	OriginHost = http_test_lib:origin_host(),
	ok = gen_tcp:send(Socket, ["HEAD ", URI, " HTTP/1.1", [13, 10],
			"Host: ", OriginHost, [13, 10],
			"Accept: audio/vnd.dolby.heaac.1", [13, 10, 13, 10]]),
	{ok, {http_response, _, 406, _}} = gen_tcp:recv(Socket, 0).

noexist() ->
	[{userdata, [{doc, "Test the HEAD method on nonexistent resource"}]}].

noexist(_Config) ->
	Reference = base64:encode_to_string(erlang:ref_to_list(make_ref())),
	URI = "/" ++ Reference ++ ".html",
	Socket = http_test_lib:connect(),
	OriginHost = http_test_lib:origin_host(),
	ok = gen_tcp:send(Socket, ["HEAD ", URI, " HTTP/1.1", [13, 10],
			"Host: ", OriginHost, [13, 10, 13, 10]]),
	{ok, {http_response, _, 404, _}} = gen_tcp:recv(Socket, 0).

nodir() ->
	[{userdata, [{doc, "Test the HEAD method on nonexistent parent resource"}]}].

nodir(_Config) ->
	Reference = base64:encode_to_string(erlang:ref_to_list(make_ref())),
	FileName = Reference ++ ".html",
	Socket = http_test_lib:connect(),
	OriginHost = http_test_lib:origin_host(),
	ok = gen_tcp:send(Socket, ["HEAD /bogus/", FileName, " HTTP/1.1", [13, 10],
			"Host: ", OriginHost, [13, 10, 13, 10]]),
	{ok, {http_response, _, 404, _}} = gen_tcp:recv(Socket, 0).

