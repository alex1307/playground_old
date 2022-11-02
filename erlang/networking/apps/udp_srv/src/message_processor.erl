%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(message_processor).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([save/2, save/3]).
-export([find/1, find/2]).
-export([find_all/1, find_all/2]).
-export([delete/1, delete/2]).
-export([get_size/0, get_size/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(SAVE(K, V), {save, K, V}).
-define(SAVE(CorrelationId, OrderNumber, Message), {save, CorrelationId, OrderNumber, Message}).

-define(DELETE(K), {delete, K}).
-define(FIND(K), {find, K}).
-define(FIND_ALL(Keys), {find_all, Keys}).
-define(DELETE_ALL, delete_all).
-define(SIZE, size).

-record(message_processor_state, {
  messages :: term(),
  ack :: term(),
  message_order:: term()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(ServerName) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [], []).

start_link() ->
  start_link(?SERVER).

init([]) ->
  {ok, #message_processor_state{
    messages = maps:new(),
    ack = sets:new(),
    message_order = ordsets:new()
  }}.

handle_call(?SAVE(K, V), _From, State = #message_processor_state{messages = M}) ->
  case maps:find(K, M) of
    error ->
      {reply, K, State#message_processor_state{messages = maps:put(K, V, M)}};
    {ok, V} ->
      {reply, ok, State};
    _ ->
      UUID = uuid:get_v4(),
      {reply, UUID, State#message_processor_state{messages = maps:put(UUID, V, M)}}
  end;

handle_call({ordered, OrderedNumber}, _From, State = #message_processor_state{message_order =  Numbers}) ->
  OrderedNumbers = ordsets:add_element(OrderedNumber, Numbers),
  {reply, OrderedNumber, State#message_processor_state{message_order =  OrderedNumbers}};

handle_call({correlation_id, CorrelationID}, _From, State = #message_processor_state{ack = IDS}) ->
  CorrelationIDs = sets:add_element(CorrelationID, IDS),
  {reply, CorrelationID, State#message_processor_state{ack =  CorrelationIDs}};


handle_call(?FIND(K), _From, State = #message_processor_state{messages = M}) ->
  case maps:find(K, M) of
    {ok, Found} ->
      {reply, Found, State};
    _Err ->
      {reply, not_found, State}
  end;

handle_call(?DELETE(K), _From, State = #message_processor_state{messages = M}) ->
  case maps:find(K, M) of
    {ok, Found} ->
      {reply, Found, State#message_processor_state{messages = maps:remove(K, M)}};
    _Err ->
      {reply, not_found, State}
  end;

handle_call(?FIND_ALL([]), _From, State) ->
  {reply, [], State};

handle_call(?FIND_ALL(Keys), _From, State = #message_processor_state{messages = M}) ->
  Found = lists:filter(
    fun(I) ->
      filter(I)
    end,
    lists:map(
      fun(K) ->
        case maps:find(K, M) of
          {ok, F} -> F;
          _NotFound -> not_found
        end
      end,
      Keys)
  ),
  {reply, Found, State};

handle_call(?SIZE, _From, State = #message_processor_state{messages = M}) ->
  {reply, maps:size(M), State};

handle_call(_Req, _From, State) ->
  {reply, invalid_pattern, State}.

handle_cast(?SAVE(K, V), State = #message_processor_state{messages = M}) ->
  case maps:find(K, M) of
    error ->
      {noreply, State#message_processor_state{messages = maps:put(K, V, M)}};
    {ok, V} ->
      {noreply, State};
    _ ->
      {noreply, State#message_processor_state{messages = maps:put(uuid:get_v4(), V, M)}}
  end;

handle_cast(?DELETE(K), State = #message_processor_state{messages = M}) ->
  {noreply, State#message_processor_state{messages = maps:remove(K, M)}};

handle_cast(_Request, State = #message_processor_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #message_processor_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #message_processor_state{}) ->
  ok.

code_change(_OldVsn, State = #message_processor_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% Internal functions
%%%===================================================================
filter(error) -> false;
filter(undefined) -> false;
filter(not_found) -> false;
filter(_) -> true.



%%%===================================================================
%%% APIs
%%%===================================================================

save(Server, K, V) when is_atom(Server) ->
  gen_server:call(Server, ?SAVE(K, V)).

delete(Server, K) when is_atom(Server) ->
  gen_server:call(Server, ?DELETE(K)).

find(Server, K) when is_atom(Server) ->
  gen_server:call(Server, ?FIND(K)).

find_all(_Server, []) ->
  [];

find_all(Server, Keys) when is_list(Keys) andalso is_atom(Server) ->
  gen_server:call(Server, ?FIND_ALL(Keys)).

get_size(Server) when is_atom(Server) ->
  gen_server:call(Server, ?SIZE).

save(K, V) -> save(?SERVER, K, V).
delete(K) -> delete(?SERVER, K).
find(K) -> find(?SERVER, K).
find_all(K) -> find_all(?SERVER, K).
get_size() -> get_size(?SERVER).

