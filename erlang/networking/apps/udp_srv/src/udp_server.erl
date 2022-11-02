%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. May 2021 13:46
%%%-------------------------------------------------------------------
-module(udp_server).
-author("alex").

-behaviour(gen_server).

-include("message_protocol.hrl").
-include_lib("utils/include/servers.hrl").

%% API
-export([start_link/1, start_link/2, start_link/3]).
-export([configuration/0, configuration/1]).
-export([configure/2, configure/3, configure_active/2]).
-export([send_and_forget/2, send_message/2, send_and_ack_message/2]).
-export([sent_bytes/1, sent_messages/1, confirmed/1]).
-export([sent_bytes/0, sent_messages/0, confirmed/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(udp_listener_state, {
  port :: integer(),
  ip :: tuple(),
  client :: list(),
  socket :: term(),
  active :: false | true | once | integer()
}).

-define(ACK_MESSAGE(CorrelationId, Payload), {ack, CorrelationId, Payload}).
-define(ACK_MESSAGE(Payload), {ack, Payload}).
-define(ORDERED_MESSAGE(OrderNumber, Payload), {ordered, OrderNumber, Payload}).
-define(SIMPLE_MESSAGE(Payload), {message, Payload}).
-define(UDP_MESSAGE(CorrelationId, OrderNumber, Payload), {udp, CorrelationId, OrderNumber, Payload}).
-define(CONFIGURE(IP, Port), {configure, IP, Port}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Port :: integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Port) ->
  start_link(Port, ?SERVER).

start_link(Server, Port) when is_atom(Server) ->
  gen_server:start_link({local, Server}, ?MODULE, [Port, {0, 0, 0, 0}], []).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Server :: atom(), Port :: integer(), IP :: {integer(), integer(), integer(), integer()}) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Server, Port, IP) ->
  gen_server:start_link({local, Server}, ?MODULE, [Port, IP], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #udp_listener_state{}} | {ok, State :: #udp_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Port, IP]) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, 500}, {recbuf, 16384}, {reuseaddr, true}, {ip, IP}]),
  lager:info("UDP server is started on port: ~p", [Port]),
  {ok, #udp_listener_state{
    port = Port,
    socket = Socket,
    client = [],
    active = 500}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #udp_listener_state{}) ->
  {reply, Reply :: term(), NewState :: #udp_listener_state{}} |
  {reply, Reply :: term(), NewState :: #udp_listener_state{}, timeout() | hibernate} |
  {noreply, NewState :: #udp_listener_state{}} |
  {noreply, NewState :: #udp_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #udp_listener_state{}} |
  {stop, Reason :: term(), NewState :: #udp_listener_state{}}).

handle_call(?CONFIGURE(IP, Port), _From, State = #udp_listener_state{client = Clients}) ->
  NewState = State#udp_listener_state{
    client = lists:append(Clients, [{IP, Port}])
  },
  {reply, ok, NewState};

handle_call(configuration, _From, State = #udp_listener_state{client = Clients}) ->
  {reply, Clients, State};


handle_call(_, _From,
    State = #udp_listener_state{
      client = []}) ->
  {reply, err_not_configured, State};



handle_call(?SIMPLE_MESSAGE(Payload), _From,
    State = #udp_listener_state{
      socket = Socket,
      client = ClientHost}) ->
  Message = udp_message:create_simple_message(Payload),
  Response = do_send(Socket, ClientHost, Message),
  {reply, Response, State};

handle_call(?ACK_MESSAGE(CorrelationId, Payload), _From,
    State = #udp_listener_state{
      socket = Socket,
      client = ClientHost}) ->
  Message = udp_message:create_ack_message(CorrelationId, Payload),
  Response = do_send(Socket, ClientHost, Message),
  {reply, Response, State};

handle_call(?ACK_MESSAGE(Payload), _From,
    State = #udp_listener_state{
      socket = Socket,
      client = ClientHost}) ->
  Now = erlang:system_time(milli_seconds),
  CorrelationId = udp_message:format_binary(binary:encode_unsigned(Now), 8),
  Message = udp_message:create_ack_message(CorrelationId, Payload),
%%  lager:info("~p: ~p Sending message: ~p", [?MODULE, ?LINE, Message]),
  case do_send(Socket, ClientHost, Message) of
    ok ->
      Now,
      {reply, Now, State};

    Err -> {reply, Err, State}
  end;



handle_call(?ORDERED_MESSAGE(OrderNumber, Payload), _From,
    State = #udp_listener_state{
      socket = Socket,
      client = ClientHost}) ->
  Message = udp_message:create_ordered_message(OrderNumber, Payload),
  Response = do_send(Socket, ClientHost, Message),
  {reply, Response, State};

handle_call({active, Active}, _From, State)
  when Active == true
  orelse Active == false
  orelse Active == once
  orelse (Active > 1 andalso Active < 32767) ->
  {reply, Active, State#udp_listener_state{active = Active}};

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #udp_listener_state{}) ->
  {noreply, NewState :: #udp_listener_state{}} |
  {noreply, NewState :: #udp_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #udp_listener_state{}}).
handle_cast(?SIMPLE_MESSAGE(Payload),
    State = #udp_listener_state{
      socket = Socket,
      client = ClientHost}) ->
  Message = udp_message:create_simple_message(Payload),
  do_send(Socket, ClientHost, Message),
  {noreply, State};

handle_cast(?ACK_MESSAGE(CorrelationId, Payload),
    State = #udp_listener_state{
      socket = Socket,
      client = ClientHost}) ->
  Message = udp_message:create_ack_message(CorrelationId, Payload),
  do_send(Socket, ClientHost, Message),
  {noreply, State};



handle_cast(_Request, State = #udp_listener_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #udp_listener_state{}) ->
  {noreply, NewState :: #udp_listener_state{}} |
  {noreply, NewState :: #udp_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #udp_listener_state{}}).
handle_info({udp_passive, Socket}, State = #udp_listener_state{active = N}) ->
%%  lager:info("~p: ~p Message: ~p", [?MODULE, ?LINE, udp_passive]),
  inet:setopts(Socket, [{active, N}, {recbuf, 32768}]),
  {noreply, State};

handle_info({udp, Socket, Host, Port, Bin}, State) ->
  Message = udp_message:process_message(Bin),
  incoming_message(Message, Socket, Host, Port),
%%  lager:info("~p: ~p Message: ~p", [?MODULE, ?LINE, Bin]),
%%  lager:info("~p: ~p Host: ~p, Port: ~p", [?MODULE, ?LINE, Host, Port]),
%%  lager:info("~p: ~p Message: ~p", [?MODULE, ?LINE, Message]),
  {noreply, State};

handle_info(Info, State = #udp_listener_state{}) ->
  lager:info("Recieved whatever:  ~p", [Info]),
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #udp_listener_state{}) -> term()).
terminate(_Reason, _State = #udp_listener_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #udp_listener_state{},
    Extra :: term()) ->
  {ok, NewState :: #udp_listener_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #udp_listener_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

configure(Server, IP = {I1, I2, I3, I4}, Port) ->
  gen_server:call(Server, ?CONFIGURE(IP, Port)).

configure(IP = {I1, I2, I3, I4}, Port) ->
  configure(?SERVER, IP, Port).

configuration(Server) ->
  gen_server:call(Server, configuration).

configuration() ->
  configuration(?SERVER).

configure_active(ServerName, Active) when
  is_atom(ServerName)
    andalso (
      Active == true
        orelse Active == false
        orelse Active == once
        orelse (Active > 1 andalso Active < 32767)
  ) ->
  gen_server:call(ServerName, {active, Active});

configure_active(_ServerName, _Active) ->
  invalid_parameters.

send_message(Server, Message)
  when is_atom(Server) andalso is_binary(Message) ->
  gen_server:call(Server, ?SIMPLE_MESSAGE(Message));

send_message(_Server, _Message) ->
  invalid_parameters.

send_message(Message) ->
  send_message(?SERVER, Message).


send_and_forget(ServerName, Message) when is_atom(ServerName) andalso is_binary(Message) ->
  gen_server:cast(ServerName, ?SIMPLE_MESSAGE(Message));

send_and_forget(_ServerName, _Message) ->
  invalid_parameters.

ack_message(ServerName, CorrelationID, Message) ->
  gen_server:call(ServerName, ?ACK_MESSAGE(CorrelationID, Message)).

ack_message(CorrelationID, Message) ->
  ack_message(?SERVER, CorrelationID, Message).


send_and_ack_message(ServerName, Message) ->
  gen_server:call(ServerName, ?ACK_MESSAGE(Message)).

send_and_ack_message(Message) ->
  send_and_ack_message(?SERVER, Message).

sent_bytes(ServerName) ->
  gen_server:call(ServerName, send_bytes).

sent_messages(ServerName) ->
  gen_server:call(ServerName, sent_messages).

confirmed(ServerName) ->
  gen_server:call(ServerName, confirmed_messages).

sent_bytes() ->
  sent_bytes(?SERVER).

sent_messages() ->
  sent_messages(?SERVER).

confirmed() ->
  confirmed(?SERVER).

do_send(_Socket, undefined, Message) ->
  err_not_configured;

do_send(_Socket, _ClientHost, error) ->
  err_invalid_message;

do_send(Socket, [], Message) when is_binary(Message) ->
  ok;

do_send(Socket, [ClientHost| T], Message) when is_tuple(ClientHost) and is_binary(Message) ->
%%  <<N:64, _/binary>> = Message,
%%  case N of
%%    N when N > 0 ->
%%      message_processor:save(?ACK_SRV, N, Message);
%%    _ -> no_ack
%%  end,
  message_counter:do_count(?OUTGOING_MESSAGE_COUNTER_SRV, Message),
  gen_udp:send(Socket, ClientHost, Message),
  do_send(Socket, T, Message);

do_send(_Socket, _ClientHost, _Err) ->
  error.

incoming_message(#message{type = ?MESSAGE, correlation_id = 0, payload = Payload}, Socket, IP, Port) ->
  message_counter:do_count(?INCOMING_MESSAGE_COUNTER_SRV, Payload),
  ok;


incoming_message(#message{type = ?MESSAGE, correlation_id = N, payload = Payload}, Socket, IP, Port) when N > 0 ->
%%  message_processor:save(?MESSAGE_SRV, N, Payload),
%%  message_processor:save(?SYNC_SRV, N, <<>>),
%%  lager:info("~p: ~p sync_mesages: ~p", [?MODULE, ?LINE, message_processor:get_size(?SYNC_SRV)]),
%%  lager:info("~p: ~p mesages: ~p", [?MODULE, ?LINE, message_processor:get_size(?MESSAGE_SRV)]),
  message_counter:do_count(?INCOMING_MESSAGE_COUNTER_SRV, Payload),
  gen_udp:send(Socket, {IP, Port}, <<?SYN/binary, N:64>>),
  ok;

incoming_message(#message{type = ?MESSAGE_SYNC, correlation_id = N}, Socket, IP, Port) when N > 0 ->
  message_counter:do_count(?INCOMING_MESSAGE_COUNTER_SRV, integer_to_binary(N)),
  gen_udp:send(Socket, {IP, Port}, <<?ACK/binary, N:64>>),
  ok;

incoming_message(#message{type = ?MESSAGE_ACK, correlation_id = N}, Socket, IP, Port) when N > 0 ->
  message_counter:do_count(?INCOMING_MESSAGE_COUNTER_SRV, integer_to_binary(N)),
  ok;

incoming_message(_Err, Socket, IP, Port) ->
  lager:error("invalid.message"),
  error.

