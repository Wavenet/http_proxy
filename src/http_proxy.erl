%%% http_proxy.erl
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
%%% @doc This library module implements the public API for the
%%% 	{@link //http_proxy. http_proxy} application.
%%%
-module(http_proxy).
-copyright('Copyright (c) 2013 Wavenet International (Pvt) Ltd.').
-author('Vance Shipley <vance@wavenet.lk>').

-include("http_proxy.hrl").

%% export the http_proxy public API
-export([start/2, stop/1]).

%%----------------------------------------------------------------------
%%  The http_proxy API
%%----------------------------------------------------------------------

-spec start(Port :: non_neg_integer(), Options :: [http_proxy_option()]) ->
	{ok, Service :: pid()} | {error, Reason :: term()}.
%% @doc Start a new HTTP proxy service.
start(Port, Options) when is_integer(Port), is_list(Options) ->
	ProxySpec = {Port, Options},
   case supervisor:start_child(http_proxy_ua_sup, [[ProxySpec]]) of
      {ok, Service} ->
			{ok, Service};
      {error, Reason} ->
         {error, Reason}
   end.

-spec stop(Service :: pid()) -> ok.
%% @doc Stop a running HTTP proxy service.
stop(Service) when is_pid(Service) ->
	case supervisor:terminate_child(http_proxy_ua_sup, Service) of
		ok ->
			ok;
		{error, not_found} ->
			exit(badarg);
		{error, Reason} ->
			exit(Reason)
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

