%%% mod_delete.erl
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

