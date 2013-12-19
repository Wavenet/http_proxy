%%% http_proxy_ua_accept_sup.erl
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
%%% @docfile "supervision.edoc"
%%%
-module(http_proxy_ua_accept_sup).
-copyright('Copyright (c) 2013 Wavenet International (Pvt) Ltd.').
-author('Vance Shipley <vance@wavenet.lk>').

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).

%%----------------------------------------------------------------------
%%  The supervisor callback
%%----------------------------------------------------------------------

-spec init(Args :: [term()]) ->
	{ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()},
			[supervisor:child_spec()]}} | ignore.
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init(Args) ->
	ChildSpecs = [server(http_proxy_ua_accept_server, Args),
			supervisor(http_proxy_ua_connect_sup, [])],
	{ok, {{one_for_all, 0, 1}, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec server(StartMod :: atom(), Args :: [term()]) ->
	supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/gen_server. gen_server} behaviour.
%% @private
%%
server(StartMod, Args) ->
	StartArgs = [StartMod, [self() | Args], []],
	StartFunc = {gen_server, start_link, StartArgs},
	{StartMod, StartFunc, permanent, 4000, worker, [StartMod]}.

-spec supervisor(StartMod :: atom(), Args :: [term()]) ->
	supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link //stdlib/supervisor. supervisor} behaviour.
%% @private
%%
supervisor(StartMod, Args) ->
	StartArgs = [StartMod, Args],
	StartFunc = {supervisor, start_link, StartArgs},
	{StartMod, StartFunc, permanent, infinity, supervisor, [StartMod]}.

