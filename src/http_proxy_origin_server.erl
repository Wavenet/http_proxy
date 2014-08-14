%%% http_proxy_origin_server.erl
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
%%% @doc This {@link //stdlib/gen_server. gen_server} behaviour callback
%%% 	module receives user agent (UA) requests from
%%%   {@link //http_proxy/http_proxy_ua_connect_fsm.
%%%   http_proxy_ua_connect_fsm} processes for resources at origin servers
%%% 	and forwards them to a
%%% 	{@link //http_proxy/http_proxy_origin_connect_fsm.
%%% 	http_proxy_origin_connect_fsm} process handling a TCP connection to 
%%% 	the origin server, starting the process if it does not yet exist.
%%%
-module(http_proxy_origin_server).
-copyright('Copyright (c) 2013 Wavenet International (Pvt) Ltd.').
-author('Vance Shipley <vance@wavenet.lk>').

-behaviour(gen_server).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-record(state,
		{origin_sup :: pid(),
		connect_sup :: pid(),
		options :: string()}).

-ifdef(debug).
-define(DEBUG, {debug, [trace]}).
-else.
-define(DEBUG, ).
-endif.

%%----------------------------------------------------------------------
%%  The http_proxy_origin_server gen_server callbacks
%%----------------------------------------------------------------------

-spec init(Args :: list()) ->
	{ok, State :: #state{}}
			| {ok, State :: #state{}, Timeout :: timeout()}
			| {stop, Reason :: term()}.
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([OriginSup, Options] = _Args) when is_pid(OriginSup) ->
	pg2:join(origin_server, self()),
	process_flag(trap_exit, true),
	{ok, #state{origin_sup = OriginSup, options = Options}, 0}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()},
		State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}}
			| {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate}
			| {noreply, NewState :: #state{}}
			| {noreply, NewState :: #state{}, timeout() | hibernate}
			| {stop, Reason :: term(), Reply :: term(), NewState :: #state{}}
			| {stop, Reason :: term(), NewState :: #state{}}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
%%
handle_call({_HttpRequest, _Head, _Body} = Request, {Pid, _Tag} = _From,
		#state{connect_sup = ConnectSup, options = Options} = State) ->
	% @todo return cached resource
	% @todo discover existing connection
	Result = supervisor:start_child(ConnectSup,
			[[Pid, Request, Options], [?DEBUG]]),
	{reply, Result, State}.

-spec handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}}
			| {noreply, NewState :: #state{}, timeout() | hibernate}
			| {stop, Reason :: term(), NewState :: #state{}}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast(stop, State) ->
	{stop, normal, State}.

-spec handle_info(Info :: timeout | term(), State::#state{}) ->
	{noreply, NewState :: #state{}}
			| {noreply, NewState :: #state{}, timeout() | hibernate}
			| {stop, Reason :: term(), NewState :: #state{}}.
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(timeout, #state{origin_sup = OriginSup} = State) ->
	Siblings = supervisor:which_children(OriginSup),
	{_, ConnectSup, _, _} = lists:keyfind(http_proxy_origin_connect_sup,
			1, Siblings),
	{noreply, State#state{connect_sup = ConnectSup}}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State::#state{}) -> any().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason, _State) ->
	ok.

-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

