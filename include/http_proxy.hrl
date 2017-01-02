%%% http_proxy.hrl
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
		path :: binary() | '*'}).
-type http_request() :: #request{}.

-record(reply,
		{status_code :: pos_integer(),
		status_string :: string(),
		head :: [{http_field(), binary()}],
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

