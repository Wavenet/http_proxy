-module(mod_put).

-export([do/1]).

-include_lib("inets/include/httpd.hrl"). 

do(#mod{method = "PUT",  request_uri = URI,
		parsed_header = RequestHeaders, entity_body = RequestBody,
		config_db = ConfigDB, data = Data}) ->
	case lists:keyfind("content-length", 1, RequestHeaders) of
		{"content-length", CL} ->
			case list_to_integer(CL) of
				ContentLength when ContentLength =:= length(RequestBody) ->
					do_put(URI, RequestBody, ConfigDB, Data);
				_ContentLength ->
					{proceed, [{response, {400, []}} | Data]}
			end;
		false ->
			{proceed, [{response, {411, []}} | Data]}
	end.

do_put(URI, RequestBody, ConfigDB, Data) ->
	DocumentRoot = httpd_util:lookup(ConfigDB, document_root),
	FileName = DocumentRoot ++ URI,
	case file:read_file_info(FileName) of
		{ok, _FileInfo} ->
			% OldEtag = httpd_util:create_etag(FileInfo),
			case write_file(FileName, RequestBody) of
				{error, Reason} ->
					error_logger:error_report([{method, "PUT"},
              		 {filename, FileName}, {entity_body, RequestBody},
						{reason, Reason}]),
					{proceed, [{response, {500, []}} | Data]};
				{etag, _} = Etag ->
					{proceed, [{response, {response, [{code, 204},
							Etag, {content_length, "0"}], []}} | Data]}
			end;
		{error, enoent} ->
			case write_file(FileName, RequestBody) of
				{error, enoent} ->
					{proceed, [{response, {404, []}} | Data]};
				{error, Reason} ->
					error_logger:error_report([{method, "PUT"},
              		 {filename, FileName}, {entity_body, RequestBody},
						{reason, Reason}]),
					{proceed, [{response, {500, []}} | Data]};
				{etag, _} = Etag ->
					{proceed, [{response, {response, [{code, 201},
							Etag, {content_length, "0"}], []}} | Data]}
			end;
	 	{error, Reason} ->
			error_logger:error_report([{method, "PUT"}, {request_uri, URI},
               {entity_body, RequestBody}, {data, Data}, {reason, Reason}]),
			{proceed, [{response, {500, []}} | Data]}
	end.

write_file(FileName, Contents) ->
	case file:write_file(FileName, Contents) of
		ok ->
			case file:read_file_info(FileName) of
				{ok, FileInfo} ->
					{etag, httpd_util:create_etag(FileInfo)};
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

