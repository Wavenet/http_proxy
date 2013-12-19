%%% http_proxy_app.erl
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
%%% @doc This {@link //stdlib/application. application} behaviour callback
%%% 	module starts and stops the {@link //http_proxy. http_proxy} application.
%%%
-module(http_proxy_app).
-copyright('Copyright (c) 2013 Wavenet International (Pvt) Ltd.').
-author('Vance Shipley <vance@wavenet.lk>').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).

-type state() :: [{Pid :: pid(), PoolSize :: pos_integer(),
		Options :: [gen_tcp:listen_option()]}].

%%----------------------------------------------------------------------
%%  The http_proxy_app aplication callbacks
%%----------------------------------------------------------------------

-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-spec start(StartType :: start_type(), StartArgs :: term()) ->
	{'ok', pid(), State :: state()} | {'error', Reason :: term()}.
%% @doc Starts the application processes.
%% @see //kernel/application:start/1
%% @see //kernel/application:start/2
%%
start(normal = _StartType, _Args) ->
	pg2:create(origin_server),
	case supervisor:start_link(http_proxy_sup, []) of
		{ok, Sup} ->
			{ok, ProxySpecs} = application:get_env(proxy),
			start1(ProxySpecs, Sup, []);
		{error, Reason} ->
			{ok, AppName} = application:get_application(),
			error_logger:error_report([atom_to_list(AppName)
					 ++ " application failed to start",
					{reason, Reason}, {module, ?MODULE}]),
			{error, Reason}
	end.
%% @hidden
start1([{Port, Options} = ProxySpec | T], Sup, Acc)
		when is_integer(Port), is_list(Options) ->
	case supervisor:start_child(http_proxy_ua_sup, [[ProxySpec]]) of
		{ok, Pid} ->
			start1(T, Sup, [{Pid, ProxySpec} | Acc]);
		{error, Reason} ->
			{error, Reason}
	end;
start1([], Sup, Acc) ->
	{ok, Options} = application:get_env(origin),
   {PoolOpt, OtherOptions} = proplists:split(Options, [pool_size]),
   PoolOption = lists:flatten(PoolOpt),
   PoolSize = proplists:get_value(pool_size, PoolOption, 1),
	start2(PoolSize, OtherOptions, Sup, Acc).
%% @hidden
start2(0, _Options, Sup, Acc) ->
	{ok, Sup, lists:reverse(Acc)};
start2(N, Options, Sup, Acc) ->
	case supervisor:start_child(http_proxy_origin_pool_sup, [[Options]]) of
		{ok, Pid} ->
			start2(N - 1, Options, Sup, [Pid | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.

-spec stop(State :: state()) -> any().
%% @doc Called after the application has stopped to clean up.
%%
stop(_State) ->
	pg2:delete(origin_server),
	ok.

-spec config_change(Changed :: [{Par :: atom(), Val :: atom()}],
		New :: [{Par :: atom(), Val :: atom()}],
		Removed :: [Par :: atom()]) -> ok.
%% @doc Called after a code  replacement, if there are any 
%% 	changes to the configuration  parameters.
%%
config_change(_Changed, _New, _Removed) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

