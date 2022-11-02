%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ack_message_listener).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-include_lib("utils/include/service_space.hrl").
-record(ack_message_listener_state, {
  last_offset :: integer(),
  last_timestamp::integer(),
  deleted::integer()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #ack_message_listener_state{last_offset = 0, last_timestamp = 0, deleted = 0}}.

handle_call(offset, _From, State = #ack_message_listener_state{last_offset = Offset}) ->
  {reply, Offset, State};

handle_call(timestamp, _From, State = #ack_message_listener_state{last_timestamp = Timestamp}) ->
  {reply, Timestamp, State};

handle_call(deleted, _From, State = #ack_message_listener_state{deleted = Deleted}) ->
  {reply, Deleted, State};

handle_call(_Request, _From, State = #ack_message_listener_state{}) ->
  {reply, ok, State}.

handle_cast(?ACK_MESSAGES(Messages), State = #ack_message_listener_state{last_offset = LastOffset, last_timestamp = LastTimestamp, deleted = All}) ->

  case process_messages(Messages, LastOffset, LastTimestamp) of
    {Offset, Timestamp, Deleted} ->
      case Offset - LastOffset of
        Deleted ->
          lager:info("Garbage collector status: OK"),
          {noreply, State#ack_message_listener_state{last_offset = Offset, last_timestamp = Timestamp, deleted = All + Deleted}};
        Incorrect ->
          lager:error("Garbage collector status: NOK. Offset: {~p, ~p}, Timestamp: {~p, ~p}, Deleted: ~p",
            [LastOffset, Offset, LastTimestamp, Timestamp, Incorrect]),
          {noreply, State#ack_message_listener_state{last_offset = Offset, last_timestamp = Timestamp, deleted = Incorrect + All}}
      end;
    ?ERROR ->
      {noreply, State#ack_message_listener_state{last_offset = LastOffset, last_timestamp = LastTimestamp}}
  end;

handle_cast(_Request, State = #ack_message_listener_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #ack_message_listener_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #ack_message_listener_state{}) ->
  ok.

code_change(_OldVsn, State = #ack_message_listener_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(process_messages([Message], LastOffset::integer(), LastTimestamp::integer()) -> {LastOffset :: integer(), LastTimestamp :: integer()}  | error | ?BAD_REQUEST
  when Message :: {kafka_message, Offset :: integer(), Key :: binary(), Payload :: binary(), Create :: atom(), Timestamp :: integer(), Headers :: list()}).
process_messages(Messages, LastOffset, LastTimestamp)
  when is_list(Messages) andalso is_integer(LastOffset) andalso is_integer(LastTimestamp) ->
  delete_all(Messages, LastOffset, LastTimestamp, 0);

process_messages(_Messages, _Offset, _Timestamp) ->
  ?BAD_REQUEST.


delete_all([], LastOffset, LastTimestamp, Deleted) when is_integer(LastOffset) andalso is_integer(LastTimestamp) ->
  {LastOffset, LastTimestamp, Deleted};

delete_all([_H = {kafka_message, CurrentOffset, Key, Payload, _Create, Timestamp, _Headers} | T], LastOffset, LastTimestamp, Deleted)
  when is_integer(CurrentOffset) andalso
  is_binary(Key) andalso
  is_binary(Payload) andalso
  is_integer(Timestamp) andalso
  is_integer(LastOffset) andalso
  is_integer(LastTimestamp) ->

  crud_db_server:cast_delete(message, Key),
  delete_all(T, max(CurrentOffset, LastOffset), max(Timestamp, LastTimestamp), Deleted + 1);

delete_all([_H  | T], LastOffset, LastTimestamp, Deleted)
  when is_list(T) andalso
  is_integer(LastOffset) andalso
  is_integer(Deleted) andalso
  is_integer(LastTimestamp) ->

  delete_all(T, LastOffset, LastTimestamp, Deleted);

delete_all(_Any, _LastOffset, _LastTimestamp, _Deleted) ->
  ?ERROR.