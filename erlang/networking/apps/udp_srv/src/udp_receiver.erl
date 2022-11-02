%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. May 2021 13:46
%%%-------------------------------------------------------------------
-module(udp_receiver).
-author("alex").

-behaviour(gen_server).
-include("message_protocol.hrl").
%% API
-export([start_link/1, start_link/2, start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(udp_listener_state, {
  port :: integer(),
  messages :: list(),
  socket :: term(),
  counter :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Port :: integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Port) ->
  start_link(Port, ?SERVER).

start_link(Port, Server) ->
  gen_server:start_link({local, Server}, ?MODULE, [Port, {0, 0, 0, 0}], []).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Port :: integer(), Server::atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Port, IP, Server) ->
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
  {ok, Socket} = gen_udp:open(Port, [binary, {recbuf, 16384},{reuseaddr, true}, {ip,IP}]),
  lager:info("UDP server is started on port: ~p", [Port]),
  {ok, #udp_listener_state{port = Port, socket = Socket, messages = [], counter = 0}}.

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
handle_call(_Request, _From, State = #udp_listener_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #udp_listener_state{}) ->
  {noreply, NewState :: #udp_listener_state{}} |
  {noreply, NewState :: #udp_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #udp_listener_state{}}).
handle_cast(_Request, State = #udp_listener_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #udp_listener_state{}) ->
  {noreply, NewState :: #udp_listener_state{}} |
  {noreply, NewState :: #udp_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #udp_listener_state{}}).
handle_info({udp, Socket, Host, Port, Bin}, State = #udp_listener_state{messages = _Messages, counter = Counter}) ->
  lager:info("~p: ~p Host: ~p, Port: ~p", [?MODULE, ?LINE, Host, Port]),
  lager:info("~p: ~p Message: ~p", [?MODULE, ?LINE, Bin]),
  #message{
   type = Type,
    correlation_id = CorrelationID
  } = udp_message:process_message(Bin),

  case CorrelationID of
    N when N > 0 ->
      gen_udp:send(Socket, {Host, Port}, <<6, CorrelationID:64>>);
    _ ->
      ok
  end,

  case Counter rem 10000 of
    0 ->
      lager:info("Pid: ~p ==> Number of processed messages: ~p", [self(), Counter]);
    _Any -> ok
  end,
  {noreply, State#udp_listener_state{counter = Counter + 1}};

handle_info(Info, State = #udp_listener_state{}) ->
%%  lager:info("Recieved whatever:  ~p", [Info]),
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
