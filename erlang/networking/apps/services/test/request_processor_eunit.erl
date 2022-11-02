%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jan 2021 8:14
%%%-------------------------------------------------------------------
-module(request_processor_eunit).
-author("alex").

-include_lib("eunit/include/eunit.hrl").
-include_lib("utils/include/mnesia_model.hrl").

-define(CONFIG_DIR, "./apps/handlers/resources").

ensure_app_is_started(_) ->
  ?assertEqual(true, is_process_alive(whereis(jiffy_srv))),
  ?assertEqual(true, is_process_alive(whereis(elavro_srv))),
  ?assertEqual(ok, elavro_srv:get_status()),
  ?assertEqual([message], elavro_srv:get_records()),
  ?assertEqual(?CONFIG_DIR, elavro_srv:get_dir()),
  ?assert(is_process_alive(whereis(request_processor))),
  ?assert(is_process_alive(whereis(crud_db_server))),
  ?_assert(is_process_alive(whereis(elavro_srv))).



process_payload_test(_D) ->
  {Status, M} = request_processor:process_payload(invalid),
  ?assertEqual(?INVALID_FUNCTION_CALL, Status),
  {S1, _M1} = request_processor:process_payload(<<"{}">>),
  ?assertEqual(?EMPTY, S1),
  {S2, _M2} = request_processor:process_payload(<<"{ not a json }">>),
  ?assertEqual(?INVALID_JSON, S2),
  {S3, _M3} = request_processor:process_payload(<<"{ \"bar\": \"foo\" }">>),
  ?assertEqual(?INVALID_AVRO, S3),
  app_supervisor:terminate_child(jiffy_srv),
  JSON = message(7007),
  {S4, _M4} = request_processor:process_payload(JSON),
  ?assertEqual(?SERVER_IS_NOT_RUNNING, S4),
  app_supervisor:start_child(jiffy_srv, jiffy_srv, start_link, []),
  {OK, Response} =  request_processor:process_payload(JSON),
  ?assertEqual(ok, OK),
  ?_assert(is_record(Response, message)).


crud_test(_ServerName) ->
  lager:info("Memory: ~p", [erlang:memory()]),
  lager:info("Memory: ~p", [erlang:memory()]),
  Json101 = message(101),
  Message = request_processor:save_message(Json101),
  Message = request_processor:find_message(Message#message.correlation_id),
  lager:info("UUID: ~p", [Message#message.correlation_id]),
%%  Json = request_processor:get_original_message(Message#message.correlation_id),
%%  lager:info("JSON: ~p", [Json]),
  OK = request_processor:delete_message(Message#message.correlation_id),
  ?_assertEqual(OK, ok).

test_suite(Data) ->
  [
    {"Ensure all apps are started", ensure_app_is_started(Data)},
    {"CRUD test", crud_test(Data)},
    {"Test process_payload method", process_payload_test(Data)}
  ].

start_balancers_test_() ->
  {inorder,
    [
      {setup, fun services_eunit_setup:setup/0, fun services_eunit_setup:cleanup/1, fun test_suite/1}
    ]
  }.

save_message(N) ->
  Message = message(N),
  request_processor:save_message(Message).

crud(N) ->
  Message = message(N),
  request_processor:save_message(Message),
  Message = request_processor:find_message(N),
  request_processor:delete_message(N).


message(N) ->
  Start = <<"{\"id\":\"">>,
  ID = integer_to_binary(N),
  Rest = <<"\",\"from\":\"James\",\"to\":\"Bond\",\"subject\":\"007\",\"body\":\"Kill them all\",\"attachment\":\"none\"}">>,
  <<Start/binary, ID/binary, Rest/binary>>.