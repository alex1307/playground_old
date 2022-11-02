%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2021 8:51
%%%-------------------------------------------------------------------
-module(load_balancer_srv).
-author("alex").

-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([send_message/2, send_message/1,
  configure_limiter/2,
  configure_delay_and_limiter/3,
  configure_delay_in_ms/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(lb_state, {
  servers :: list(),
  executors :: list(),
  delay :: integer(),
  rate :: integer(),
  limiter :: integer(),
  type :: atom()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(ServerName :: atom(), Servers :: list(), Type :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ServerName, Servers, Type)
  when is_atom(ServerName) andalso is_list(Servers) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [Servers, Type], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #lb_state{}} | {ok, State :: #lb_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Servers, call]) when is_list(Servers) ->
  {ok, #lb_state{servers = Servers,
    type = call,
    delay = 2,
    rate = 1,
    limiter = 20,
    executors = Servers}};

init([Servers, cast]) when is_list(Servers) ->
  {ok, #lb_state{servers = Servers, type = cast, executors = Servers}};

init([_Any]) ->
  {stop, invalid_parameters}.


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #lb_state{}) ->
  {reply, Reply :: term(), NewState :: #lb_state{}} |
  {reply, Reply :: term(), NewState :: #lb_state{}, timeout() | hibernate} |
  {noreply, NewState :: #lb_state{}} |
  {noreply, NewState :: #lb_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #lb_state{}} |
  {stop, Reason :: term(), NewState :: #lb_state{}}).
handle_call({configure, Delay, Limiter}, _From, State) ->
  {reply, State, State#lb_state{limiter = Limiter, delay = Delay}};

handle_call({delay, Delay}, _From, State) ->
  {reply, State, State#lb_state{delay = Delay}};

handle_call({limiter, Limiter}, _From, State) ->
  {reply, State, State#lb_state{limiter = Limiter}};

handle_call(Request, _From, State = #lb_state{executors = Executors, servers = Servers, rate = Rate, limiter = Limiter}) when Rate =< Limiter ->
  case execute_call(Executors, Request, Servers) of
    {Response, Reduced} when is_list(Reduced) ->
      {reply, Response, State#lb_state{executors = Reduced, rate = Rate + 1}};
    _Any ->
      {reply, error, State}
  end;
handle_call(Request, _From, State = #lb_state{executors = Executors, servers = Servers, rate = Rate, limiter = Limiter, delay = Delay}) when Rate > Limiter ->
  case execute_call(Executors, Request, Servers, Delay) of
    {Response, Reduced} when is_list(Reduced) ->
      {reply, Response, State#lb_state{executors = Reduced, rate = 1}};
    _Any ->
      {reply, error, State}
  end.


%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #lb_state{}) ->
  {noreply, NewState :: #lb_state{}} |
  {noreply, NewState :: #lb_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #lb_state{}}).
handle_cast(Request, State = #lb_state{executors = Executors, servers = Servers, rate = Rate, limiter = Limiter, delay = Delay}) when Rate > Limiter ->
  case execute_call(Executors, Request, Servers) of
    {Response, Reduced} when is_list(Reduced) ->
      {noreply, State#lb_state{executors = Reduced, rate = Rate + 1}};
    _Any ->
      {noreply, State}
  end;
handle_cast(Request, State = #lb_state{executors = Executors, servers = Servers, rate = Rate, limiter = Limiter, delay = Delay}) ->
  case execute_call(Executors, Request, Servers, Delay) of
    {Response, Reduced} when is_list(Reduced) ->
      {noreply, State#lb_state{executors = Reduced, rate = 1}};
    _Any ->
      {noreply, State}
  end.


handle_info(_Info, State) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #lb_state{}) -> term()).
terminate(_Reason, _State = #lb_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #lb_state{},
    Extra :: term()) ->
  {ok, NewState :: #lb_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #lb_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_message(ServerName, Message) ->
  gen_server:cast(ServerName, {send, Message}).

send_message(Message) ->
  send_message(?SERVER, Message).

execute_call([Server | T], Request, Source, DelayMs) ->
  timer:sleep(DelayMs),
  execute_call([Server | T], Request, Source).


execute_call([Server | T], Request, Source) when T == [] ->
  Response = gen_server:call(Server, Request),
  {Response, Source};

execute_call([Server | T], Request, _Source) when T =/= [] ->
  Response = gen_server:call(Server, Request),
  {Response, T};


execute_call(_Servers, _Request, _Source) ->
  error.

execute_cast([Server | T], Option, Source) when T == [] ->
  Response = gen_server:cast(Server, Option),
  {Response, Source};

execute_cast([Server | T], Option, _Source) when T =/= [] ->
  Response = gen_server:cast(Server, Option),
  {Response, T};


execute_cast([_Server | _T], _Option, _Source) ->
  error.

configure_delay_in_ms(ServerName, Millis) ->
  gen_server:call(ServerName, {delay, Millis}).

configure_limiter(ServerName, Limiter) ->
  gen_server:call(ServerName, {limiter, Limiter}).

configure_delay_and_limiter(ServerName, Millis, Limiter) ->
  gen_server:call(ServerName, {configure, Millis, Limiter}).


