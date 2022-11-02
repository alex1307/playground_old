%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2021 8:18
%%%-------------------------------------------------------------------
-module(garbage_collector_eunit).
-author("alex").

-include_lib("eunit/include/eunit.hrl").
-include_lib("utils/include/mnesia_model.hrl").

simple_test() ->
  ?assert(true).

setup() ->
  services_eunit_setup:setup(),
  {ok, Pid} = ack_message_listener:start_link(),
  test_data(100, []).

cleanup(Data) ->
  services_eunit_setup:cleanup(Data).

ensure_all_services_are_started(Data) ->
  ?assert(is_process_alive(whereis(ack_message_listener))),
  ?assert(is_process_alive(whereis(jiffy_srv))),
  ?assert(is_process_alive(whereis(elavro_srv))),
  ?assert(is_process_alive(whereis(crud_db_server))),
  ?_assert(length(Data) == 100).

crud_test(_D) ->
  Json1001 = message(1001),
  Message = request_processor:save_message(Json1001),
  Message = request_processor:find_message(Message#message.correlation_id),
  gen_server:cast(ack_message_listener, ?ACK_MESSAGES([to_kafka_message(Message)])),
  ?_assertEqual(not_found, request_processor:find_message(Message#message.correlation_id)).

bulk_message_test(Data) ->
  Messages = lists:map(
    fun(J) ->
      Message = request_processor:save_message(J),
      Message
    end, Data),
  ?assertEqual(100, length(Messages)),
  lists:foreach(
    fun(M) ->
      ?assertEqual(M, request_processor:find_message(M#message.correlation_id))
    end,
    Messages),

  KafkaMessages = lists:map(
    fun(M) ->
      to_kafka_message(M)
    end, Messages),
  ?assertEqual(100, length(KafkaMessages)),
  gen_server:cast(ack_message_listener, ?ACK_MESSAGES(KafkaMessages)),
  lists:foreach(
    fun(M) ->
      timer:sleep(10),
      ?assertEqual(not_found, request_processor:find_message(M#message.correlation_id))
    end,
    Messages),
  ?_assert(true).

test_data(0, Messages) ->
  Messages;

test_data(N, L) ->
  Messages = lists:append(L, [message(N)]),
  test_data(N - 1, Messages).


message(N) ->
  Start = <<"{\"id\":\"">>,
  ID = integer_to_binary(N),
  Rest = <<"\",\"from\":\"James\",\"to\":\"Bond\",\"subject\":\"007\",\"body\":\"Kill them all\",\"attachment\":\"none\"}">>,
  <<Start/binary, ID/binary, Rest/binary>>.

to_kafka_message(Message) ->
  {kafka_message, 1, Message#message.correlation_id, Message#message.payload, create, erlang:system_time(), []}.

test_suite(TestData) ->
  [
    {"Ensure all services are started and test data is loaded", ensure_all_services_are_started(TestData)},
    {"Create message and clear it using garbage collectro serviece", crud_test(TestData)},
    {"Bulk crud test with 100 messages", bulk_message_test(TestData)}
  ].

start_balancers_test_() ->
  {inorder,
    [
      {setup, fun setup/0, fun cleanup/1, fun test_suite/1}
    ]
  }.