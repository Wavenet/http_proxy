%%% http_proxy_origin_connect_fsm.erl
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
%%% @doc This {@link //stdlib/gen_fsm. gen_fsm} behaviour callback
%%% 	module handles outgoing TCP connections to origin servers.
%%% @todo Receive new requests while already open.
-module(http_proxy_origin_connect_fsm).
-copyright('Copyright (c) 2013 Wavenet International (Pvt) Ltd.').
-author('Vance Shipley <vance@wavenet.lk>').

-behaviour(gen_fsm).

-include("http_proxy.hrl").

%% export the callbacks needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

%% export the gen_fsm state callbacks
-export([connecting/2, idle/2, response/2, head/2, body/2]).

-record(statedata,
		{socket :: inet:socket(),
		host :: string(),
		port :: pos_integer(),
		version :: http_version(),
		queue = queue:new() :: queue:queue({pid(), #request{},
				[http_field()], binary() | [binary()]}) | {[], []},
		method :: http_method(),
		head = [] :: [http_field()],
		body :: binary() | [binary()],
		trailer = [] :: [http_field()],
		expected_length :: non_neg_integer()
				| {chunk, undefined | non_neg_integer()},
		buf = <<>> :: binary(),
		status_code :: pos_integer(),
		status_string :: binary(),
		clients :: [pid()],
		options :: [gen_tcp:connect_option()]}).

%%----------------------------------------------------------------------
%%  The http_proxy_origin_connect_fsm gen_server callbacks
%%----------------------------------------------------------------------

-spec init(Args :: [term()]) ->
	{ok, StateName :: atom(), StateData :: #statedata{}}
			| {ok, StateName :: atom(), StateData :: #statedata{}, timeout() | hibernate}
			| {stop, Reason :: term()} | ignore.
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init([Pid, {#request{host = Host, port = Port} = Request,
		Head, Body}, Options] = _Args) ->
	% {Lopts, TcpOptions} = proplists:split(Options, [pool_size, server_name]),
	Queue = queue:in({Pid, Request, Head, Body}, queue:new()),
	NewStateData = #statedata{host = Host, port = Port,
			queue = Queue, clients = [Pid], options = Options},
	process_flag(trap_exit, true),
	{ok, connecting, NewStateData, 0}.

-spec connecting(Event :: timeout | term(), StateData :: #statedata{}) ->
	{next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
			| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
				timeout() | hibernate}
			| {stop, Reason :: term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%% 	gen_fsm:send_event/2} in the <b>connecting</b> state.
%% @private
%%
connecting(timeout, #statedata{host = Host, port = Port,
		options = TcpOptions} = StateData) ->
	case gen_tcp:connect(Host, Port,
			[{packet, http_bin}, {active, once} | TcpOptions]) of
		{ok, Socket} ->
			{next_state, idle, StateData#statedata{socket = Socket}, 0};
		{error, _Reason} ->
			reply_error(StateData#statedata{status_code = 504})
	end.

-spec idle(Event :: timeout | term(), StateData :: #statedata{}) ->
	{next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
			| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
				timeout() | hibernate}
			| {stop, Reason :: term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%% 	gen_fsm:send_event/2} in the <b>idle</b> state.
%% @private
%%
idle(timeout, #statedata{queue = {[], []}} = StateData) ->
	{stop, normal, StateData};
idle(timeout, #statedata{queue = Queue, clients = Clients,
		socket = Socket} = StateData) ->
	{Client, #request{method = Method, path = URI},
			Head, Body} = queue:get(Queue),
	case lists:member(Client, Clients) of
		true ->
			NewStateData = StateData#statedata{method = Method, head = [],
					body = undefined, expected_length = undefined},
			case send_request(Socket, Method, URI, Head, Body) of
				ok ->
					{next_state, response, NewStateData};
				{error, Reason} ->
					{stop, Reason, NewStateData}
			end;
		false ->
			NewStateData = StateData#statedata{queue = queue:drop(Queue),
					clients = lists:delete(Client, Clients)},
			{next_state, idle, NewStateData}
	end.

-spec response(Event :: timeout | term(), StateData :: #statedata{}) ->
	{next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
			| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
				timeout() | hibernate}
			| {stop, Reason :: term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%% 	gen_fsm:send_event/2} in the <b>response</b> state.
%% @private
%%
response(timeout, StateData) ->
	{stop, normal, StateData}.

-spec head(Event :: timeout | term(), StateData :: #statedata{}) ->
	{next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
			| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
				timeout() | hibernate}
			| {stop, Reason :: term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%% 	gen_fsm:send_event/2} in the <b>head</b> state.
%% @private
%%
head(_Event, StateData) ->
	{stop, not_implemented, StateData}.

-spec body(Event :: timeout | term(), StateData :: #statedata{}) ->
	{next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
			| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
				timeout() | hibernate}
			| {stop, Reason :: term(), NewStateData :: #statedata{}}.
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%% 	gen_fsm:send_event/2} in the <b>body</b> state.
%% @private
%%
body(_Event, StateData) ->
	{stop, not_implemented, StateData}.

-spec handle_event(Event :: term(), StateName :: atom(),
		StateData :: #statedata{}) ->
	{next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
			| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
				timeout() | hibernate}
			| {stop, Reason :: term(), NewStateData :: #statedata{}}.
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2.
%% 	gen_fsm:send_all_state_event/2}.
%% @see //stdlib/gen_fsm:handle_event/3
%% @private
%%
handle_event(_Event, _StateName, StateData) ->
	{stop, not_implemented, StateData}.

-spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
		StateName :: atom(), StateData :: #statedata{}) ->
	{reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()}
			| {reply, Reply :: term(), NextStateName :: atom(),
				NewStateData :: #statedata{}, timeout() | hibernate}
			| {next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
			| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
				timeout() | hibernate}
			| {stop, Reason :: term(), Reply :: term(),
				NewStateData :: #statedata{}}
			| {stop, Reason :: term(), NewStateData :: #statedata{}}.
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(_Event, _From, _StateName, StateData) ->
	{stop, not_implemented, StateData}.

-spec handle_info(Info :: term(), StateName :: atom(),
		StateData :: #statedata{}) ->
	{next_state, NextStateName :: atom(), NewStateData :: #statedata{}}
			| {next_state, NextStateName :: atom(), NewStateData :: #statedata{},
				timeout() | hibernate}
			| {stop, Reason :: normal | term(), NewStateData :: #statedata{}}.
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info({http, Socket, {http_response, HttpVersion,
		StatusCode, StatusString}}, response = _StateName,
		#statedata{socket = Socket, queue = Queue} = StateData)
		when StatusCode >= 100, StatusCode < 200 ->
	NewStateData = StateData#statedata{status_code = StatusCode,
			status_string = StatusString, version = HttpVersion},
	{Client, _Request, _Head, _Body} = queue:get(Queue),
	Reply = #reply{status_code = StatusCode, status_string = StatusString},
	gen_fsm:send_event(Client, Reply),
	set_active(response, NewStateData);
handle_info({http, Socket, {http_response, HttpVersion,
		StatusCode, HttpString}}, response = _StateName,
		#statedata{socket = Socket} = StateData) ->
	NewStateData = StateData#statedata{status_code = StatusCode,
			status_string = HttpString, version = HttpVersion},
	set_active(head, NewStateData);
handle_info({http, Socket,
		{http_header, _, 'Transfer-Encoding', _, Encoding}}, head = _StateName,
		#statedata{socket = Socket, status_code = StatusCode,
		head = Head} = StateData) when StatusCode >= 200, StatusCode < 300 ->
	% @todo parse encoding type
	NewStateData = StateData#statedata{expected_length = {chunk, undefined},
			head = [{'Transfer-Encoding', Encoding} | Head]},
	set_active(head, NewStateData);
handle_info({http, Socket, {http_header, _, 'Content-Length', _, _}},
		head = _StateName, #statedata{socket = Socket, status_code = StatusCode,
		expected_length = {chunk, _}} = StateData) when StatusCode >= 200, StatusCode < 300 ->
	set_active(head, StateData);
handle_info({http, Socket,
		{http_header, _, 'Content-Length', _, Length}}, head = _StateName,
		#statedata{socket = Socket, method = 'HEAD',  status_code = StatusCode,
		head = Head} = StateData) when StatusCode >= 200, StatusCode < 300 ->
	NewStateData = StateData#statedata{
			head = [{'Content-Length', Length} | Head]},
	set_active(head, NewStateData);
handle_info({http, Socket, {http_header, _, 'Content-Length', _, Length}},
		head = _StateName, #statedata{socket = Socket, head = Head} = StateData) 
		when Length /= <<"0">> ->
	NewStateData = StateData#statedata{
			expected_length = binary_to_integer(Length),
			head = [{'Content-Length', Length} | Head]},
	set_active(head, NewStateData);
handle_info({http, Socket, {http_header, _, HttpField, _, Value}},
		head = _StateName,
		#statedata{socket = Socket, head = Head} = StateData) ->
	NewStateData = StateData#statedata{head = [{HttpField, Value} | Head]},
	set_active(head, NewStateData);
handle_info({http, Socket, http_eoh}, head = _StateName,
		#statedata{socket = Socket, expected_length = undefined} = StateData) ->
	reply(StateData);
handle_info({http, Socket, http_eoh}, head = _StateName,
		#statedata{socket = Socket} = StateData) ->
	set_active(body, StateData);
handle_info({http, Socket, {http_error, HttpString}},
		StateName, #statedata{socket = Socket} = StateData) ->
	%% @todo forward error
	error_logger:error_report(["HTTP error",
			{module, ?MODULE}, {state, StateName},
			{socket, Socket}, {error, HttpString}]),
	{stop, HttpString, StateData};
handle_info({tcp, Socket, Data}, body = _StateName,
		#statedata{socket = Socket, body = undefined,
		expected_length = {chunk, undefined}, buf = Buf} = StateData) ->
	case binary:split(<<Buf/binary, Data/binary>>, <<$\r, $\n>>) of
		[H | []] ->
			NewStateData = StateData#statedata{buf = H},
			set_active(body, NewStateData);
		[H | [Rest]] ->
			% @todo parse chunk-extension
			[Hex | _Extensions] = binary:split(H, <<$;>>, [global]),
			{ok, [ChunkSize], _} = io_lib:fread("~16u", binary_to_list(Hex)),
			NewStateData = StateData#statedata{buf = <<>>,
					expected_length = {chunk, ChunkSize}},
			handle_info({tcp, Socket, Rest}, body, NewStateData)
	end;
handle_info({tcp, Socket, Data}, body = _StateName,
		#statedata{socket = Socket, body = undefined,
		expected_length = {chunk, 0}, buf = Buf, trailer = Trailer} = StateData) ->
	NewBuf = <<Buf/binary, Data/binary>>,
	case erlang:decode_packet(httph_bin, NewBuf, []) of
		{ok, {http_header, _, HttpField, _, Value}, Rest} ->
			NewStateData = StateData#statedata{buf = <<>>,
					trailer = [{HttpField, Value} | Trailer]},
			handle_info({tcp, Socket, Rest}, body, NewStateData);
		{ok, http_eoh, Rest} ->
			NewStateData = StateData#statedata{buf = Rest},
			reply(NewStateData);
		{more, _Length} ->
			NewStateData = StateData#statedata{buf = NewBuf},
			set_active(body, NewStateData);
		{error, Reason} ->
			{stop, Reason}
	end;
handle_info({tcp, Socket, Data}, body = _StateName,
		#statedata{socket = Socket, body = undefined, queue = Queue,
		expected_length = {chunk, ChunkSize}, buf = Buf} = StateData) ->
	case <<Buf/binary, Data/binary>> of
		<<>> ->
			set_active(body, StateData);
		<<Chunk:ChunkSize/binary, $\r, $\n, Rest/binary>> ->
			{value, {Client, _Request, _Head, _Body}} = queue:peek(Queue), 
			gen_fsm:send_event(Client, Chunk),
			NewStateData = StateData#statedata{buf = <<>>,
					expected_length = {chunk, undefined}},
			handle_info({tcp, Socket, Rest}, body, NewStateData);
		Rest ->
			NewStateData = StateData#statedata{buf = Rest},
			set_active(body, NewStateData)
	end;
handle_info({tcp, Socket, Body}, body = _StateName,
		#statedata{socket = Socket, body = undefined,
		expected_length = Length} = StateData)
		when size(Body) =:= Length ->
	NewStateData = StateData#statedata{body = Body},
	reply(NewStateData);
handle_info({tcp, Socket, Part}, body = _StateName,
		#statedata{socket = Socket, body = Body,
		expected_length = Length} = StateData)
		when size(Part) =:= Length ->
	NewStateData = StateData#statedata{body = lists:reverse([Part | Body])},
	reply(NewStateData);
handle_info({tcp, Socket, Part}, body = _StateName,
		#statedata{socket = Socket, expected_length = Length,
		body = undefined} = StateData) when size(Part) < Length ->
	NewStateData = StateData#statedata{expected_length = Length - size(Part),
		body = [Part]},
	set_active(body, NewStateData);
handle_info({tcp, Socket, Part}, body = _StateName,
		#statedata{socket = Socket, expected_length = Length,
		body = Body} = StateData) when size(Part) < Length ->
	NewStateData = StateData#statedata{expected_length = Length - size(Part),
		body = [Part | Body]},
	set_active(body, NewStateData);
handle_info({tcp, Socket, Data}, StateName,
		#statedata{socket = Socket} = StateData) ->
	error_logger:error_report(["Unexpected tcp data",
			{module, ?MODULE}, {state, StateName},
			{socket, Socket}, {data, Data}]),
	set_active(StateName, StateData);
handle_info({tcp_closed, Socket}, _StateName,
		#statedata{socket = Socket} = StateData) ->
	{stop, normal, StateData#statedata{socket = undefined, clients = []}};
handle_info({tcp_error, Socket, Reason},
		_StateName, #statedata{socket = Socket} = StateData) ->
	{stop, {tcp_error, Reason}, StateData};
handle_info({'EXIT', Client, _Reason}, StateName,
		#statedata{clients = Clients} = StateData) ->
	NewStateData = StateData#statedata{clients = lists:delete(Client, Clients)},
	{next_state, StateName, NewStateData};
handle_info(Other, StateName, StateData) ->
	error_logger:error_report(["Unexpected info received",
			{module, ?MODULE}, {state, StateName}, {info, Other}]),
	{stop, unexpected, StateData}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		StateName :: atom(), StateData :: #statedata{}) ->
	any().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate(_Reason, _StateName, #statedata{socket = Socket})
		when is_port(Socket) ->
	gen_tcp:close(Socket);
terminate(_Reason, _StateName, _StateData) ->
	ok.

-spec code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
		StateData :: term(), Extra :: term()) ->
	{ok, NextStateName :: atom(), NewStateData :: #statedata{}}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
set_active(body, #statedata{socket = Socket,
		expected_length = {chunk, 0}} = StateData) ->
	case inet:setopts(Socket, [binary, {packet, http_bin}, {active, once}]) of
		ok ->
			{next_state, body, StateData};
		{error, Reason} ->
			{stop, Reason, StateData}
	end;
set_active(body, #statedata{socket = Socket} = StateData) ->
	case inet:setopts(Socket, [binary, {packet, raw}, {active, once}]) of
		ok ->
			{next_state, body, StateData};
		{error, Reason} ->
			{stop, Reason, StateData}
	end;
set_active(NextState, #statedata{socket = Socket} = StateData) ->
	case inet:setopts(Socket, [{packet, http_bin}, {active, once}]) of
		ok ->
			{next_state, NextState, StateData};
		{error, Reason} ->
			{stop, Reason, StateData}
	end.
%% @hidden
set_active(NextState, Timeout, #statedata{socket = Socket} = StateData) ->
	case inet:setopts(Socket, [{packet, http_bin}, {active, once}]) of
		ok ->
			{next_state, NextState, StateData, Timeout};
		{error, Reason} ->
			{stop, Reason, StateData}
	end.

%% @hidden
reply(#statedata{expected_length = {chunk, undefined}, queue = Queue,
		status_code = StatusCode, status_string = StatusString,
		head = Head, body = undefined} = StateData) ->
	{value, {Client, _Request, _Head, _Body}} = queue:peek(Queue), 
	Reply = #reply{status_code = StatusCode,
			status_string = StatusString, head = Head},
	gen_fsm:send_event(Client, Reply),
	set_active(body, StateData);
reply(#statedata{expected_length = {chunk, 0}, queue = Queue,
		body = undefined, trailer = Trailer, clients = Clients} = StateData) ->
	{{value, {Client, _Request, _Head, _Body}}, NewQueue} = queue:out(Queue), 
	gen_fsm:send_event(Client, Trailer),
	unlink(Client),
	NewClients = lists:delete(Client, Clients),
	NewStateData = StateData#statedata{queue = NewQueue, head = [],
			body = undefined, trailer = [], expected_length = undefined,
			status_code = undefined, status_string = undefined,
			clients = NewClients},
	case queue:is_empty(NewQueue) of
		true ->
			set_active(idle, 4000, NewStateData);
		false ->
			set_active(idle, 0, NewStateData)
	end;
reply(#statedata{queue = Queue, status_code = StatusCode,
		status_string = StatusString, head = Head, body = Body,
		clients = Clients} = StateData) ->
	{{value, {Client, _Request, _Head, _Body}}, NewQueue} = queue:out(Queue), 
	Reply = #reply{status_code = StatusCode, status_string = StatusString,
			head = Head, body = Body},
	gen_fsm:send_event(Client, Reply),
	unlink(Client),
	NewClients = lists:delete(Client, Clients),
	NewStateData = StateData#statedata{queue = NewQueue,
			head = [], body = undefined, expected_length = undefined,
			status_code = undefined, status_string = undefined,
			clients = NewClients},
	case queue:is_empty(NewQueue) of
		true ->
			set_active(idle, 4000, NewStateData);
		false ->
			set_active(idle, 0, NewStateData)
	end.

%% @hidden
reply_error(#statedata{queue = Queue, status_code = StatusCode} = StateData) ->
	{Client, _Request, _Head, _Body} = queue:get(Queue), 
	Reply = #reply{status_code = StatusCode},
	gen_fsm:send_event(Client, Reply),
	unlink(Client),
	{stop, normal, StateData}.

%% @hidden
send_request(Socket, Method, URI, Head, Body) ->
	Request = http_proxy_util:encode_request(Method, URI, Head, Body),
	gen_tcp:send(Socket, Request).

