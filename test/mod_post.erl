-module(mod_post).

-export([do/1]).

-include_lib("inets/include/httpd.hrl"). 

do(#mod{method = "POST",  absolute_uri = AbsoluteURI,
		request_uri = RequestURI, parsed_header = RequestHeaders,
		entity_body = RequestBody, config_db = ConfigDB, data = Data}) ->
	case lists:keyfind("content-length", 1, RequestHeaders) of
		{"content-length", CL} ->
			case list_to_integer(CL) of
				ContentLength when ContentLength =:= length(RequestBody) ->
					do_post(AbsoluteURI, RequestURI, RequestBody, ConfigDB, Data);
				_ContentLength ->
					{proceed, [{response, {400, []}} | Data]}
			end;
		false ->
			{proceed, [{response, {411, []}} | Data]}
	end.

do_post(AbsoluteURI, RequestURI, Contents, ConfigDB, Data) ->
	DocumentRoot = httpd_util:lookup(ConfigDB, document_root),
	Reference = base64:encode_to_string(erlang:ref_to_list(make_ref())),
	{Path, Location} = case tl(RequestURI) of
		$/ ->
			{RequestURI ++ Reference, AbsoluteURI ++ Reference};
		_ ->
			{RequestURI ++ "/" ++ Reference, AbsoluteURI ++ "/" ++ Reference}
	end,
	FileName = DocumentRoot ++ Path,
	case file:write_file(FileName, Contents) of
		ok ->
			case file:read_file_info(FileName) of
				{ok, FileInfo} ->
					Etag = httpd_util:create_etag(FileInfo),
					{proceed, [{response, {response, [{code, 201},
							{location, Location}, {etag, Etag},
							{content_length, "0"}], []}} | Data]};
				{error, Reason} ->
         		error_logger:error_report([{method, "POST"},
							{absolute_uri, AbsoluteURI},
							{request_uri, RequestURI},
							{data, Data}, {reason, Reason}]),
					{proceed, [{response, {500, []}} | Data]}
			end;
		{error, enoent} ->
			{proceed, [{response, {404, []}} | Data]};
		{error, Reason} ->
        	error_logger:error_report([{method, "POST"},
					{absolute_uri, AbsoluteURI},
					{request_uri, RequestURI},
					{data, Data}, {reason, Reason}]),
			{proceed, [{response, {500, []}} | Data]}
	end.

