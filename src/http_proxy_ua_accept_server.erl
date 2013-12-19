%%% http_proxy_ua_accept_server.erl
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
%%% 	module accepts connections on a listening socket and starts an
%%% 	{@link //http_proxy/http_proxy_ua_connect_fsm. http_proxy_ua_connect_fsm}
%%% 	process to handle it.
%%%
-module(http_proxy_ua_accept_server).
-copyright('Copyright (c) 2013 Wavenet International (Pvt) Ltd.').
-author('Vance Shipley <vance@wavenet.lk>').

-behaviour(gen_server).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-record(state,
		{accept_sup :: pid(),
		socket :: inet:socket(),
		acceptor :: term(),
		connect_sup :: pid(),
		server :: string()}).

%%----------------------------------------------------------------------
%%  The http_proxy_ua_accept_server gen_server callbacks
%%----------------------------------------------------------------------

-spec init(Args :: list()) ->
	{ok, State :: #state{}}
			| {ok, State :: #state{}, Timeout :: timeout()}
			| {stop, Reason :: term()}.
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([AcceptSup, ListenSocket, Server] = _Args)
		when is_pid(AcceptSup), is_port(ListenSocket) ->
	process_flag(trap_exit, true),
	{ok, #state{accept_sup = AcceptSup, socket = ListenSocket,
			server = Server}, 0}.

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
handle_call(_Request, _From, State) ->
	{stop, not_implemented, State}.

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
handle_info(timeout, #state{accept_sup = AcceptSup} = State) ->
	Siblings = supervisor:which_children(AcceptSup),
	{_, ConnectSup, _, _} = lists:keyfind(http_proxy_ua_connect_sup,
			1, Siblings),
	do_accept(State#state{connect_sup = ConnectSup});
handle_info({inet_async, ListenSocket, Ref, {ok, ConnectSocket}},
		#state{socket = ListenSocket, acceptor = Ref} = State) ->
	case set_opts(ListenSocket, ConnectSocket) of
		ok ->
			start_connect(ConnectSocket, State);
		{error, Reason} ->
			{stop, Reason, State}
	end;
handle_info({'EXIT', Socket, Reason}, #state{socket = Socket} = State) ->
	{stop, Reason, State#state{socket = undefined}}.


-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State::#state{}) -> any().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason, #state{socket = Socket}) ->
	gen_tcp:close(Socket).

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

%% @hidden
start_connect(ConnectSocket, #state{connect_sup = ConnectSup,
		server = Server} = State) ->
	case supervisor:start_child(ConnectSup,
			[[ConnectSocket, Server], [{debug, [trace]}]]) of
		{ok, Child} ->
			case gen_tcp:controlling_process(ConnectSocket, Child) of
				ok ->
					case inet:setopts(ConnectSocket, [{active, once}]) of
						ok ->
							do_accept(State);
						{error, Reason} ->
							{stop, Reason, State}
					end;
				{error, Reason} ->
					{stop, Reason, State}
			end;
		{error, Reason} ->
			{stop, Reason, State}
	end.

%% @doc Copied from {@link //kernel/prim_inet. prim_inet} module.
%% @hidden
do_accept(#state{socket = ListenSocket} = State) ->
	case prim_inet:async_accept(ListenSocket, -1) of
		{ok, Ref} ->
			{noreply, State#state{acceptor = Ref}};
		{error, Reason} ->
			{stop, Reason, State}
	end.

%% @doc Copied from {@link //kernel/prim_inet. prim_inet} module.
%% @hidden
set_opts(ListenSocket, ConnectSocket) ->
	true = inet_db:register_socket(ConnectSocket, inet_tcp),
	OptionList = [active, nodelay, keepalive, delay_send, priority, tos],
	case prim_inet:getopts(ListenSocket, OptionList) of
		{ok, ListenOptions} ->
			case prim_inet:setopts(ConnectSocket, ListenOptions) of
				ok ->
					ok;
				Error ->
					gen_tcp:close(ConnectSocket),
					Error
			end;
		Error ->
			gen_tcp:close(ConnectSocket),
			Error
	end.

