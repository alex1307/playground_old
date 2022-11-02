-define(BAD_REQUEST, bad_request).
-define(ACK_MESSAGES(Messages), {ack, Messages}).
-define(ERROR, error).
-define(OK, ok).
-define(NOK, nok).
-define(INVALID_SRV_NAME, invalid_server_name).
-define(NOT_FOUND, not_found).
-define(SERVER_IS_NOT_RUNNING, server_is_not_running).
-define(INVALID_SERVICE_PAYLOAD, invalid_service_payload).
-define(INVALID_JSON, invalid_json).
-define(EMPTY, empty).
-define(INVALID_AVRO, invalid_avro).
-define(INVALID_FUNCTION_CALL, invalid_function_call).
-define(RECORD_NOT_FOUND, record_not_found).

-define(IS_PROCESS_ALIVE(Pid), (is_pid(whereis(Pid)) andalso is_process_alive(whereis(Pid))) ).

-define(VALID, valid).
-define(INVALID, invalid).
-define(UNDEFINED, undefined).
-define(MILLIS, milli_seconds).

-define(GET_FIELDS(Record),
  case Record of
    message -> record_info(fields, message);
    _ -> error
  end).
