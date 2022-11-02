%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Feb 2021 8:23
%%%-------------------------------------------------------------------
-module(payload_validator).
-author("alex").

-include_lib("utils/include/service_space.hrl").

%% API
-export([
  process_json/1,
  json_as_tuple_to_avro_binary/2,
  process_json_to_avro/2
]).
-define(RESPONSE(Status, JsonMessage), {Status, JsonMessage}).

-spec(process_json(binary()) -> {Code, ErrorMessageOrTupleList}
  when
  Code :: ?OK | ?ERROR | ?INVALID_FUNCTION_CALL | ?INVALID_JSON | ?SERVER_IS_NOT_RUNNING | ?EMPTY,
  ErrorMessageOrTupleList :: binary() | list()).
process_json(Binary) when is_binary(Binary) ->
  process_json_response(jiffy_srv:binary_to_tuple_list(Binary));

process_json(_NotABinary) ->
  ?RESPONSE(?INVALID_FUNCTION_CALL, <<"{\"error\":\"JSON function call missmatch\"}">>).

process_json_response(?INVALID_JSON) ->
  ?RESPONSE(?INVALID_JSON, <<"{\"error\":\"invalid_json\"}">>);

process_json_response(?SERVER_IS_NOT_RUNNING) ->
  ?RESPONSE(?SERVER_IS_NOT_RUNNING, <<"{\"error\":\"temporary_unavailable\"}">>);

process_json_response([]) ->
  ?RESPONSE(?EMPTY, []);

process_json_response(L) when is_list(L) ->
  ?RESPONSE(?OK, L);

process_json_response(UnsupportedResponse) ->
  lager:error("Internal service error. Unsupported response ~p", [UnsupportedResponse]),
  ?RESPONSE(?ERROR, <<"{\"error\":\"n/a\"}">>).



json_as_tuple_to_avro_binary(_AvroRecordName, []) ->
  ?RESPONSE(?EMPTY, []);

json_as_tuple_to_avro_binary(AvroRecordName, L) when is_list(L) andalso not is_atom(AvroRecordName) ->
  ?RESPONSE(?RECORD_NOT_FOUND, []);

json_as_tuple_to_avro_binary(AvroRecordName, PropList)
  when is_atom(AvroRecordName) andalso is_list(PropList) ->
  process_avro(elavro_srv:encode(AvroRecordName, PropList));

json_as_tuple_to_avro_binary(_AvroRecordName, _PropList) ->
  ?RESPONSE(?INVALID_FUNCTION_CALL, <<"{\"error\":\"json_avro service mismatch\"}">>).


process_avro(?INVALID_AVRO) ->
  ?RESPONSE(?INVALID_AVRO, <<"{\"error\":\"invalid_avro\"}">>);

process_avro(?SERVER_IS_NOT_RUNNING) ->
  ?RESPONSE(?SERVER_IS_NOT_RUNNING, <<"{\"error\":\"temporary_unavailable\"}">>);

process_avro(?RECORD_NOT_FOUND) ->
  {?RECORD_NOT_FOUND, <<"{\"error\":\"unsupported avro model\"}">>};

process_avro(?INVALID_FUNCTION_CALL) ->
  {?INVALID_FUNCTION_CALL, <<"{\"error\":\"avro service missmatch\"}">>};

process_avro(Binary) when is_binary(Binary) ->
  ?RESPONSE(?OK, Binary);

process_avro(UnsupportedResponse) ->
  lager:error("Internal service error. Unsupported response ~p", [UnsupportedResponse]),
  ?RESPONSE(?ERROR, <<"{\"error\":\"n/a\"}">>).

process_json_to_avro(AvroRecord, Binary) when is_atom(AvroRecord) andalso is_binary(Binary) ->
  Response = ?RESPONSE(Status, Payload) = process_json(Binary),
  lager:info("JSON TO AVRO Status: ~p; Response: ~p", [Status, Payload]),
  case Status of
    ?OK ->
      json_as_tuple_to_avro_binary(AvroRecord, Payload);
    _Err ->
      Response
  end;

process_json_to_avro(_AvroRecord, _Binary) ->
  ?RESPONSE(?INVALID_FUNCTION_CALL, <<"{\"error\":\"JSON function call missmatch\"}">>).