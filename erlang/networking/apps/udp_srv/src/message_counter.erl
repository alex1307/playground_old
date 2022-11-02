%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(message_counter).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([do_count/1, do_count/2]).
-export([apply_interval/0, apply_interval/1]).
-export([get_interval_config/1, get_interval_config/0]).
-export([get_totals/1, get_totals/0]).
-export([set_interval/1, set_interval/2]).
-export([cancel_interval/0, cancel_interval/1]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MESSAGE(Message), {message, Message}).
-define(RATE(Timestamp), {rate, Timestamp}).
-define(STATISTIC, statistic).
-define(APPLY_INTERVAL(ServerName, Interval), {apply_interval, ServerName, Interval}).
-define(CANCEL_INTERVAL, cancel_interval).

-record(kpi_state, {
  counter :: integer(),
  binary_size :: integer(),
  statistic :: term(),
  interval_ref :: term(),
  interval_ms :: integer(),
  interval_counter :: integer()
}).

-record(kpi, {
  created_on :: integer(),
  count :: integer(),
  size :: integer()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(ServerName) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [], []).

start_link() ->
  start_link(?SERVER).

init([]) ->
  {ok, #kpi_state{
    statistic = maps:new(),
    counter = 0,
    binary_size = 0,
    interval_ms = 0,
    interval_counter = 0
  }}.

handle_call(?MESSAGE(Message), _From, State = #kpi_state{statistic = M, counter = Counter, binary_size = BinSize}) ->
  {reply, ok, State#kpi_state{counter = Counter + 1, binary_size = BinSize + size(Message)}};

handle_call(?RATE(Timestamp), _From,
    State = #kpi_state{statistic = M, counter = Counter, binary_size = BinSize, interval_counter = IC}) ->
  Statistic = process(Timestamp, Counter, BinSize, M),
  {reply, ok, State#kpi_state{counter = 0, binary_size = 0, statistic = Statistic, interval_counter = IC + 1}};

handle_call(totals, _From, State = #kpi_state{statistic = Statistic, counter = Count, binary_size = Size}) ->
  {reply, collect_totals(Count, Size, Statistic), State};

handle_call(interval_config, _From, State = #kpi_state{interval_ms = MS, interval_counter = Count}) ->
  {reply, {MS, Count}, State};


handle_call(?APPLY_INTERVAL(ServerName, Interval), _From, State = #kpi_state{interval_ref = Ref}) ->
  case set_interval(Interval, ServerName, Ref) of
    NewState = #kpi_state{} ->
      {reply, Interval, NewState};
    Err ->
      lager:info("~p: [~p,~p] Error: ~p", [?MODULE, ?FUNCTION_NAME, ?LINE, Err]),
      {reply, error, State}
  end;
handle_call(?CANCEL_INTERVAL, _From, State = #kpi_state{interval_ref = Ref}) ->
  case timer:cancel(Ref) of
    {ok, cancel} ->
      {reply, ok, #kpi_state{
        interval_ms = 0,
        interval_counter = 0,
        counter = 0,
        binary_size = 0,
        statistic = maps:new()
      }};
    Err ->
      lager:info("~p: [~p,~p] Error: ~p", [?MODULE, ?FUNCTION_NAME, ?LINE, Err]),
      {reply, error, State}
  end;


handle_call(?STATISTIC, _From, State = #kpi_state{statistic = M}) ->
  {reply, maps:size(M), State};

handle_call(_Req, _From, State) ->
  {reply, invalid_pattern, State}.

handle_cast(?MESSAGE(Message), State = #kpi_state{statistic = M, counter = Counter, binary_size = BinSize}) ->
  {noreply, State#kpi_state{counter = Counter + 1, binary_size = BinSize + size(Message)}};

handle_cast(?RATE(Timestamp),
    State = #kpi_state{statistic = M, counter = Counter, binary_size = BinSize, interval_counter = IC}) ->
  Statistic = process(Timestamp, Counter, BinSize, M),
  {noreply, State#kpi_state{counter = 0, binary_size = 0, statistic = Statistic, interval_counter = IC + 1}};

handle_cast(_Request, State = #kpi_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #kpi_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #kpi_state{}) ->
  ok.

code_change(_OldVsn, State = #kpi_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_interval(ServerName, Millis) ->
  gen_server:call(ServerName, ?APPLY_INTERVAL(ServerName, Millis)).

set_interval(Millis) ->
  set_interval(?SERVER, Millis).

cancel_interval(ServerName) ->
  gen_server:call(ServerName, ?CANCEL_INTERVAL).

cancel_interval() ->
  cancel_interval(?SERVER).

apply_interval(ServerName) when is_atom(ServerName) ->
  case whereis(ServerName) of
    Pid when is_pid(Pid) ->
      Timestamp = erlang:system_time(millisecond),
      gen_server:call(ServerName, ?RATE(Timestamp));
    _Undefined -> not_started
  end;

apply_interval(_ServerName) ->
  lager:error("~p: ~p invalid.server.name"),
  error.

apply_interval() ->
  apply_interval(?SERVER).

get_totals(ServerName) when is_atom(ServerName) ->
  gen_server:call(ServerName, totals).

get_totals() ->
  gen_server:call(?SERVER, totals).


get_interval_config(ServerName) ->
  gen_server:call(ServerName, interval_config).

get_interval_config() ->
  get_interval_config(?SERVER).

do_count(ServerName, Message) ->
  gen_server:call(ServerName, ?MESSAGE(Message)).

do_count(Message) ->
  do_count(?SERVER, Message).

process(_Timestamp, 0, _Size, CurrentStatistic) ->
  CurrentStatistic;

process(Timestamp, Count, Size, CurrentStatistic)
  when is_integer(Timestamp) andalso is_integer(Count) andalso is_integer(Size) ->
  case maps:is_key(Timestamp, CurrentStatistic) of
    true ->
      #kpi{
        count = FCount,
        size = FSize
      } = maps:get(Timestamp, CurrentStatistic),
      create_statistic(Timestamp, FCount + Count, FSize + Size, CurrentStatistic);
    _False ->
      create_statistic(Timestamp, Count, Size, CurrentStatistic)
  end;

process(_Timestamp, _Counter, _BinSize, CurrentStatistic) ->
  lager:error("~p: ~p Invalid parameter(s)", [?MODULE, ?LINE]),
  CurrentStatistic.

collect_totals(Count, Size, Statistic) ->
  Values = maps:values(Statistic),
  TotalCount = lists:sum(
    lists:map(
      fun(#kpi{count = Value}) ->
        case Value of
          N when is_integer(N) -> N;
          _Any -> 0
        end
      end,
      Values)),
  TotalSize = lists:sum(
    lists:map(
      fun(#kpi{size = Value}) ->
        case Value of
          N when is_integer(N) -> N;
          _Any -> 0
        end
      end,
      Values)),
  ActiveTimeInSeconds = max(1, lists:sum(
    lists:map(
      fun(#kpi{count = Value}) ->
        case Value of
          N when is_integer(N) andalso N > 0 -> 1;
          _Any -> 0
        end
      end,
      Values))),
  Average = ((TotalCount + Count) / ActiveTimeInSeconds),
  [
    {count, TotalCount},
    {size_in_bytes, TotalSize},
    {size_in_MB, trunc(TotalSize / 1048576)},
    {count_per_second, trunc(Average)},
    {mb_per_second, trunc(TotalSize / (ActiveTimeInSeconds * 1048576))},
    {duration, ActiveTimeInSeconds}
  ].

create_statistic(_Timestamp, 0, _Size, Statistic) ->
  Statistic;

create_statistic(Timestamp, Count, Size, Statistic)
  when is_integer(Timestamp)
  andalso is_integer(Count)
  andalso is_integer(Size) ->
  maps:put(Timestamp, #kpi{count = Count, size = Size, created_on = erlang:system_time(millisecond)}, Statistic);

create_statistic(_Timestamp, _Count, _Size, Statistic) ->
  lager:error("~p: ~p Invalid parameters."),
  Statistic.

set_interval(IntervalInMillis, ServerName, Reference = {_Interval, Ref})
  when is_reference(Ref)
  andalso is_integer(IntervalInMillis)
  andalso is_atom(ServerName) ->

  case timer:cancel(Reference) of
    {ok, cancel} ->
      {ok, TRef} = timer:apply_interval(IntervalInMillis, ?MODULE, apply_interval, [ServerName]),
      #kpi_state{
        counter = 0,
        binary_size = 0,
        interval_ref = TRef,
        interval_ms = IntervalInMillis,
        interval_counter = 0,
        statistic = maps:new()
      };
    _Err ->
      error
  end;

set_interval(IntervalInMillis, ServerName, undefined) when is_integer(IntervalInMillis) andalso is_atom(ServerName) ->
  {ok, TRef} = timer:apply_interval(IntervalInMillis, ?MODULE, apply_interval, [ServerName]),
  lager:info("interval (~p ms) for server ~p has been configured. Ref: ~p", [IntervalInMillis, ServerName, TRef]),
  #kpi_state{
    counter = 0,
    binary_size = 0,
    interval_ref = TRef,
    statistic = maps:new(),
    interval_counter = 0,
    interval_ms = IntervalInMillis
  };

set_interval(_IntervalInMillis, _ServerName, _) ->
  lager:error("~p:~p_~p invalid.parameters", {?MODULE, ?FUNCTION_NAME, ?LINE}),
  error.