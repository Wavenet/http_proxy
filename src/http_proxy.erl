%%% http_proxy.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  @author Vance Shipley <vance@wavenet.lk>
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

