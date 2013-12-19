-module(mod_delete).

-export([do/1]).

-include_lib("inets/include/httpd.hrl"). 

do(#mod{method = "DELETE",  request_uri = URI,
		parsed_header = RequestHeaders, config_db = ConfigDB, data = Data}) ->
	DocumentRoot = httpd_util:lookup(ConfigDB, document_root),
	FileName = DocumentRoot ++ URI,
	case file:delete(FileName) of
		ok ->
			{proceed, [{response, {204, []}} | Data]};
		{error, enoent} ->
			{proceed, [{response, {404, []}} | Data]};
		{error, enotdir} ->
			{proceed, [{response, {404, []}} | Data]};
		{error, Reason} ->
         error_logger:error_report([{method, "DELETE"}, {uri, URI},
               {headers, RequestHeaders}, {data, Data}, {reason, Reason}]),
			{proceed, [{response, {500, []}} | Data]}
	end.

