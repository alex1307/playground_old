%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Feb 2021 11:18
%%%-------------------------------------------------------------------
-module(payload_validator_eunit).
-author("alex").
-include_lib("eunit/include/eunit.hrl").
-include_lib("utils/include/service_space.hrl").
%% API
-export([]).

-define(CONFIG_DIR, "./apps/handlers/resources").

ensure_app_is_started(_) ->
  ?assertEqual(true, is_process_alive(whereis(jiffy_srv))),
  ?assertEqual(true, is_process_alive(whereis(elavro_srv))),
  ?assertEqual(ok, elavro_srv:get_status()),
  ?assertEqual([message], elavro_srv:get_records()),
  ?assertEqual(?CONFIG_DIR, elavro_srv:get_dir()),
  ?_assert(is_process_alive(whereis(elavro_srv))).

valid_json_test([Json, _Avro]) ->
  [Json, _Avro] = eunit_setup:setup(),
  Content = lists:map(
    fun({_F, C}) ->
      C
    end,
    Json),
  json_are_valid(Content).

invalid_json_test([_Json, _]) ->
  OK = lists:foreach(
    fun(_) ->
      {Response, Message} = payload_validator:process_json(uuid:get_v4()),
      ?assertEqual(?INVALID_JSON, Response),
      ?assertEqual(<<"{\"error\":\"invalid_json\"}">>, Message)
    end,
    lists:seq(1, 10)),
  ?_assertEqual(ok, OK).

server_is_not_running_test_() ->
  {Response, Message} = payload_validator:process_json(uuid:get_v4()),
  ?assertEqual(?SERVER_IS_NOT_RUNNING, Response),
  ?_assertEqual(Message, <<"{\"error\":\"temporary_unavailable\"}">>).

invalid_service_call_test_() ->
  {Response, Message} = payload_validator:process_json(invalid_payload),
  ?assertEqual(?INVALID_FUNCTION_CALL, Response),
  ?_assertEqual(Message, <<"{\"error\":\"JSON function call missmatch\"}">>).

elavro_server_empty_test_() ->
  {Response, Message} = payload_validator:json_as_tuple_to_avro_binary(message, []),
  ?assertEqual(?EMPTY, Response),
  ?_assertEqual(Message, []).

elavro_server_is_not_running_test_() ->
  {Response, Message} = payload_validator:json_as_tuple_to_avro_binary(message, [<<"">>]),
  ?assertEqual(?SERVER_IS_NOT_RUNNING, Response),
  ?_assertEqual(Message, <<"{\"error\":\"temporary_unavailable\"}">>).

elavro_invalid_service_call_test_() ->
  {Response, Message} = payload_validator:json_as_tuple_to_avro_binary(message, invalid_payload),
  ?assertEqual(?INVALID_FUNCTION_CALL, Response),
  ?_assertEqual(Message, <<"{\"error\":\"json_avro service mismatch\"}">>).

avro_invalid_function_all_test() ->
  {R, M} = payload_validator:process_json_to_avro(<<>>, <<>>),
  ?assertEqual(?INVALID_FUNCTION_CALL, R),
  ?_assertEqual(M, []).

avro_record_not_found_test(_D) ->
  {R, M} = payload_validator:process_json_to_avro(another_message, <<"{ \"bar\": \"foo\" }">>),
  ?assertEqual(?RECORD_NOT_FOUND, R),
  ?_assertEqual(M, <<"{\"error\":\"unsupported avro model\"}">>).

json_to_avro_is_not_running_test_() ->
  {Response, Message} = payload_validator:process_json_to_avro(message, <<"">>),
  ?assertEqual(?SERVER_IS_NOT_RUNNING, Response),
  ?_assertEqual(<<"{\"error\":\"temporary_unavailable\"}">>, Message).

json_to_avro_invalid_json_test(_) ->
  {Response, Message} = payload_validator:process_json_to_avro(message, <<"invalid">>),
  ?assertEqual(?INVALID_JSON, Response),
  ?_assertEqual(<<"{\"error\":\"invalid_json\"}">>, Message).

json_to_avro_empty_json_test(_) ->
  {Response, Message} = payload_validator:process_json_to_avro(message, <<"{}">>),
  ?assertEqual(?EMPTY, Response),
  ?_assertEqual([], Message).

json_to_avro_invalid_avro_test(_) ->
  {Response, Message} = payload_validator:process_json_to_avro(message, <<"{ \"bar\": \"foo\" }">>),
  ?assertEqual(?INVALID_AVRO, Response),
  ?_assertEqual(<<"{\"error\":\"invalid_avro\"}">>, Message).


json_are_valid([]) ->
  ?_assert(true);

json_are_valid([Content | T]) ->
  {ok, Response} = payload_validator:process_json(Content),
  ?assert(is_list(Response)),
  json_are_valid(T).


empty_json_test(_) ->
  {Res, Mess} = payload_validator:process_json(<<"{}">>),
  ?_assertEqual(?EMPTY, Res),
  ?_assertEqual(Mess, []).



eunit_test_(Data) ->
  lager:info("===== Testing Data: ~p ====", [Data]),
  [
    {"Ensure handlers app istarted", ensure_app_is_started(Data)},
    {"Test invalid payload", invalid_json_test(Data)},
    {"JSON is valid test", valid_json_test(Data)},
    {"Empty {} JSON test", empty_json_test(Data)},
    {"Record not found test", avro_record_not_found_test(Data)},
    {"Test json -> avro. Invalid JSON", json_to_avro_invalid_json_test(Data)},
    {"Test json -> avro. Invalid JSON", json_to_avro_empty_json_test(Data)},
    {"Conver json to avro. Invalid Avro is expected", json_to_avro_invalid_avro_test(Data)}
  ].

handlers_test_() ->
  {inorder,
    [
      {setup, fun eunit_setup:setup/0, fun eunit_setup:cleanup/1, fun eunit_test_/1}
    ]
  }.