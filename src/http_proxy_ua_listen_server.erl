%%% http_proxy_ua_listen_server.erl
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
%%% 	module opens a listen socket on a port and starts the accept pool.
%%%
-module(http_proxy_ua_listen_server).
-copyright('Copyright (c) 2013 Wavenet International (Pvt) Ltd.').
-author('Vance Shipley <vance@wavenet.lk>').

-behaviour(gen_server).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-record(state,
		{sup :: pid(),
		port :: pos_integer(),
		socket :: inet:socket(),
		pool_size :: pos_integer(),
		options :: [gen_tcp:listen_option()],
		accept_pool_sup :: pid(),
		server :: string()}).

%%----------------------------------------------------------------------
%%  The http_proxy_ua_listen_server gen_server callbacks
%%----------------------------------------------------------------------

-spec init(Args :: list()) ->
	{ok, State :: #state{}}
			| {ok, State :: #state{}, Timeout :: timeout()}
			| {stop, Reason :: term()}.
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([Sup, {Port, Options}] = _Args) when is_pid(Sup),
		is_integer(Port), is_list(Options) ->
	{Lopts, TcpOptions} = proplists:split(Options, [pool_size, server_name]),
	ProxyOptions = lists:flatten(Lopts),
	PoolSize = proplists:get_value(pool_size, ProxyOptions, 1),
	{ok, AppName} = application:get_application(),
	{ok, Version} = application:get_key(vsn),
	ServerName = atom_to_list(AppName) ++ "/" ++ Version,
	Server = proplists:get_value(server_name, ProxyOptions, ServerName),
	case gen_tcp:listen(Port, [{reuseaddr, true}, {packet, http_bin},
			{active, false} | TcpOptions]) of
		{ok, Socket} when Port =:= 0 ->
			case inet:port(Socket) of
				{ok, ListenPort} ->
					process_flag(trap_exit, true),
					NewState = #state{sup = Sup, port = ListenPort,
							pool_size = PoolSize, options = Options,
							socket = Socket, server = Server},
					{ok, NewState, 0};
				{error, Reason} ->
					{stop, Reason}
			end;
		{ok, Socket} ->
			process_flag(trap_exit, true),
			NewState = #state{sup = Sup, port = Port, pool_size = PoolSize,
					options = Options, socket = Socket, server = Server},
			{ok, NewState, 0};
		{error, Reason} ->
			{stop, Reason}
	end.

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
handle_info(timeout, #state{sup = PortSup, pool_size = PoolSize} = State) ->
	Siblings = supervisor:which_children(PortSup),
	{_, PoolSup, _, _} = lists:keyfind(http_proxy_ua_pool_sup,
			1, Siblings),
	start_pool(PoolSize, State#state{accept_pool_sup = PoolSup});
handle_info({'EXIT', Socket, Reason}, #state{socket = Socket} = State) ->
	{stop, Reason, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State::#state{}) -> any().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason, #state{socket = Socket}) when is_port(Socket) ->
	gen_tcp:close(Socket);
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

start_pool(0, State) ->
	{noreply, State};
start_pool(N, #state{socket = Socket, accept_pool_sup = PoolSup,
		server = Server} = State) ->
	case supervisor:start_child(PoolSup, [[Socket, Server]]) of
		{ok, _Child} ->
			start_pool(N - 1, State);
		{error, Reason} ->
			{stop, Reason, State}
	end.

