%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jan 2016 7:00 PM
%%%-------------------------------------------------------------------
-author("alex").

-include("service_space.hrl").

-define(CREATE_COMMAND, <<"CREATE">>).
-define(UPDATE_COMMAND, <<"UPDATE">>).
-define(DELETE_COMMAND, <<"DELETE">>).
-define(GET_COMMAND, <<"SELECT">>).

-define(STATUS_SUCCESS, 'SUCCESS').
-define(STATUS_ERROR, 'ERROR').
-define(STATUS_NOT_FOUND, 'NOT_FOUND').

-define(DEF_DISC_COPIES, {disc_copies, [erlang:node()]}).
-define(PT_TIMEOUT, 1000).

-define(MAX_DB_INSTANCES, 5).
-define(DB_SERVER, db_server).

-record(event_log, {
  correlation_id :: string(),
  payload :: binary(),
  created_on :: integer()
}).


-record(message, {
  correlation_id :: binary(),
  ref::reference(),
  type :: atom(),
  status :: atom(),
  payload :: binary()
}).

-record(acknowledged, {
  reference :: reference(),
  created_on :: integer()
}).

-define(MESSAGE_PROPS, [{record_name, message}, {attributes, record_info(fields, message)}]).
-define(ACKNOWLEDGED_PROPS, [{record_name, acknowledged}, {attributes, record_info(fields, acknowledged)}]).
-define(EVENT_LOG_PROPS, [{record_name, event_log}, {attributes, record_info(fields, event_log)}]).

-define(GET_PROPS(TableName),

  case TableName of
    message -> ?MESSAGE_PROPS;
    acknowledged -> ?ACKNOWLEDGED_PROPS;
    event_log -> ?EVENT_LOG_PROPS;
    _ -> []
  end).


-define(MNESIA_TABLES,
  [message, acknowledged, event_log]).

-define(RESULTS_NOT_FOUND, []).
-define(SINGLE_RECORD_TABLES, []).


-define(GET_ID(Payload, Entity),
  case Entity of
    message -> Payload#message.correlation_id;
    event_log -> Payload#event_log.correlation_id;
    acknowledged -> Payload#acknowledged.reference;
    _ -> error
  end).