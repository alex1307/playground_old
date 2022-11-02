
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 16-Dec-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(handlers_eunit).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-include_lib("eunit/include/eunit.hrl").
-include_lib("utils/include/service_space.hrl").
-define(CONFIG_DIR, "./apps/handlers/resources").

ensure_app_is_started(_D) ->
  ?assertEqual(true, is_process_alive(whereis(jiffy_srv))),
  ?assertEqual(true, is_process_alive(whereis(elavro_srv))),
  ?assertEqual(ok, elavro_srv:get_status()),
  ?assertEqual([attachment,contact,message,message_orig], elavro_srv:get_records()),
  ?assertEqual(?CONFIG_DIR, elavro_srv:get_dir()),
  ?_assert(is_process_alive(whereis(elavro_srv))).

invalid_json(_D) ->
  Bin = <<"this is not a JSON. This is a text!!!!!">>,
  Response = jiffy_srv:binary_to_tuple_list(Bin),
  ?_assertEqual(?INVALID_JSON, Response).

%%
%%
%% Decode JSON (result is tuple list)
%% Encode decoded JSON (tuple list) to AVRO
%% Compare encoded AVRO (binary) with AVRO content
%%
%%
decode_json_then_encode_it_and_compare_it_with_avro([JsonFiles, AvroFiles]) ->
  ?assertEqual(invalid_function_call, elavro_srv:encode(model, {})),
  ?assertEqual(record_not_found, elavro_srv:encode(model, [{}])),
  OK = lists:foreach(
    fun({N, C}) ->
      TL = jiffy_srv:binary_to_tuple_list(C),
      lager:info("==TL==> ~p <====TL=====", [TL]),
      Encoded = elavro_srv:encode(message, TL),
      lager:info("======> ~p <===========", [Encoded]),
      Target = proplists:get_value(N, AvroFiles),
      ?assertEqual(Encoded, Target)
    end,
    JsonFiles),
  ?_assertEqual(ok, OK).


%%
%%
%% Decode binary Avro
%% Decode JSON
%% Compare decoded (tuple lists) results
%%
%%
decode_avro_content_and_compare_it_with_decoded_json([JsonFiles, AvroFiles]) ->
  OK = lists:foreach(
    fun({N, C}) ->
      Source = elavro_srv:decode(message, C),
      Json = proplists:get_value(N, JsonFiles),
      Target = jiffy_srv:binary_to_tuple_list(Json),
      lists:foreach(
        fun({K, V}) ->
          ?assertEqual(V, proplists:get_value(K, Source))
        end,
        Target)
    end,
    AvroFiles),
  ?_assertEqual(OK, ok).

encode_json(_) ->
  Source = [{<<"foo">>, <<"bar">>}],
  Result = <<"{\"foo\":\"bar\"}">>,
  Response = jiffy_srv:tuple_list_to_binary(Source),
  ?_assertEqual(Result, Response).

decode_json(_) ->
  Source = <<"{\"foo\":\"bar\"}">>,
  Result = [{<<"foo">>, <<"bar">>}],
  Response = jiffy_srv:binary_to_tuple_list(Source),
  ?_assertEqual(Result, Response).

%%
%%
%% Convert JSON data to AVRO data
%% Compare encoded AVRO (binary) with AVRO content
%%
%%
transform_json_to_avro([JsonFiles, AvroFiles]) ->
  OK = lists:foreach(
    fun({N, C}) ->
      Source = jiffy_srv:json_to_avro(message, C),
      Target = proplists:get_value(N, AvroFiles),
      ?assertEqual(Source, Target)
    end,
    JsonFiles
  ),
  ?_assertEqual(OK, ok).

transform_avro_to_json([JsonFiles, AvroFiles]) ->
  OK = lists:foreach(
    fun({N, C}) ->
      Source = elavro_srv:avro_to_json(message, C),
      SourceTupleList = jiffy_srv:binary_to_tuple_list(Source),
      Json = proplists:get_value(N, JsonFiles),
      TargetTupleList = jiffy_srv:binary_to_tuple_list(Json),
      lists:foreach(
        fun({K, V}) ->
          ?assertEqual(V, proplists:get_value(K, SourceTupleList))
        end,
        TargetTupleList)
    end,
    AvroFiles
  ),
  ?_assertEqual(OK, ok).

eunit_test_(Data) ->
  lager:info("===== Testing Data: ~p ====", [Data]),
  [
    {"Ensure handlers app istarted", ensure_app_is_started(Data)},
    {"Test invalid payload", invalid_json(Data)},
    {"Decode JSON to tuples and encode it to AVRO", decode_json_then_encode_it_and_compare_it_with_avro(Data)}
  ].

handlers_test_() ->
  {inorder,
    [
      {setup, fun eunit_setup:setup/0, fun eunit_setup:cleanup/1, fun eunit_test_/1}
    ]
  }.
