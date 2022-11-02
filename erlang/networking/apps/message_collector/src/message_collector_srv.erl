%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(message_collector_srv).

-behaviour(gen_server).

-export([
  start_link/0,
  start_link/1,
  start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([send_message/2,
  get_last_message/1,
  get_message/2,
  get_message_count/1]).

-define(SERVER, ?MODULE).

-record(message_collector_srv_state, {
  max_capacity :: integer(),
  counter :: integer(),
  seq::list(),
  messages :: term()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  start_link(?SERVER).


start_link(ServerName) ->
  start_link(ServerName, 100000).

start_link(ServerName, MAX_CAPACITY) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [MAX_CAPACITY], []).

init([MAX_CAPACITY]) ->
  {ok, #message_collector_srv_state{
    max_capacity = MAX_CAPACITY -1,
    counter = 0,
    seq = lists:seq(0, MAX_CAPACITY-1),
    messages = array:new(MAX_CAPACITY, {default, <<>>})
  }}.

handle_call({collect, Msg}, _From, State = #message_collector_srv_state{
  counter = Counter,
  max_capacity = Max,
  messages = Messages
}) when Counter < Max ->
  Next = Counter + 1,
  Updated = array:set(Counter, Msg, Messages),
  {reply, Next, State#message_collector_srv_state{messages = Updated, counter = Next}};

handle_call({collect, Msg}, _From, State = #message_collector_srv_state{
  counter = Max,
  max_capacity = Max,
  messages = Messages
})  ->
  lager:info("Flushing: counter: ~p, length: ~p", [Max, array:size(Messages)]),
  flush(State#message_collector_srv_state{messages = array:set(Max, Msg, Messages)}),
  {reply, 0, State#message_collector_srv_state{messages = array:new(Max + 1, {default, <<>>}), counter = 0}};

handle_call(flush, _From, State = #message_collector_srv_state{
  max_capacity = Max,
  messages = Messages
})  ->
  lager:info("Flushing: counter: ~p, length: ~p", [Max, array:size(Messages)]),
  flush(State),
  {reply, 0, State#message_collector_srv_state{messages = array:new(Max + 1, {default, <<>>}), counter = 0}};


handle_call(current_state, _From, State)  ->
  {reply, State, State};

handle_call(count, _From, State = #message_collector_srv_state{counter = Count})  ->
  {reply, Count, State};

handle_call(last, _From, State = #message_collector_srv_state{counter = Count, messages = Messages})  ->
  {reply, array:get(Count, Messages), State};

handle_call({get, Index}, _From, State = #message_collector_srv_state{messages = Messages})  ->
  {reply, array:get(Index, Messages), State};

handle_call(_Request, _From, State = #message_collector_srv_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #message_collector_srv_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #message_collector_srv_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #message_collector_srv_state{}) ->
  ok.

code_change(_OldVsn, State = #message_collector_srv_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


flush(_State = #message_collector_srv_state{messages = Messages})  ->
    Cached = array:to_list(Messages),
    scenarios:send_bulk_of_messages(worker1, Cached).

send_message(Server, Message) ->
  gen_server:call(Server, {collect, Message}).

get_message_count(Server) ->
  gen_server:call(Server, count).

get_last_message(Server) ->
  gen_server:call(Server, last).

get_message(Server, Index) ->
  gen_server:call(Server, {get, Index}).