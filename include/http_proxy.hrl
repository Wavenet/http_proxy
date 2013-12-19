%%% http_proxy.hrl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%% 

-type http_proxy_option() :: {pool_size, pos_integer()}
		| {server_name, string()} | gen_tcp:listen_option().

-type http_origin_option() :: {pool_size, pos_integer()}
		| gen_tcp:connect_option().

-record(request,
		{method :: http_method(),
		scheme :: http | https,
		host :: string(),
		port :: pos_integer(),
		path :: binary()}).
-type http_request() :: #request{}.

-record(reply,
		{status_code :: pos_integer(),
		status_string :: string(),
		head :: [http_field()],
		body :: binary() | [binary()]}).
-type http_reply() :: #reply{}.

-type http_method() :: 'OPTIONS'
           | 'GET'
           | 'HEAD'
           | 'POST'
           | 'PUT'
           | 'DELETE'
           | 'TRACE'
           | binary().

-type http_uri() :: '*'
        | {absoluteURI,
           http | https,
           Host :: binary(),
           Port :: inet:port_number() | undefined,
           Path :: binary()}
        | {scheme, Scheme :: binary(), binary()}
        | {abs_path, binary()}
        | binary().

-type http_version() :: {1, 0} | {1, 1}.

-type http_field() :: 'Cache-Control'
          | 'Connection'
          | 'Date'
          | 'Pragma'
          | 'Transfer-Encoding'
          | 'Upgrade'
          | 'Via'
          | 'Accept'
          | 'Accept-Charset'
          | 'Accept-Encoding'
          | 'Accept-Language'
          | 'Authorization'
          | 'From'
          | 'Host'
          | 'If-Modified-Since'
          | 'If-Match'
          | 'If-None-Match'
          | 'If-Range'
          | 'If-Unmodified-Since'
          | 'Max-Forwards'
          | 'Proxy-Authorization'
          | 'Range'
          | 'Referer'
          | 'User-Agent'
          | 'Age'
          | 'Location'
          | 'Proxy-Authenticate'
          | 'Public'
          | 'Retry-After'
          | 'Server'
          | 'Vary'
          | 'Warning'
          | 'Www-Authenticate'
          | 'Allow'
          | 'Content-Base'
          | 'Content-Encoding'
          | 'Content-Language'
          | 'Content-Length'
          | 'Content-Location'
          | 'Content-Md5'
          | 'Content-Range'
          | 'Content-Type'
          | 'Etag'
          | 'Expires'
          | 'Last-Modified'
          | 'Accept-Ranges'
          | 'Set-Cookie'
          | 'Set-Cookie2'
          | 'X-Forwarded-For'
          | 'Cookie'
          | 'Keep-Alive'
          | 'Proxy-Connection'
          | binary().

