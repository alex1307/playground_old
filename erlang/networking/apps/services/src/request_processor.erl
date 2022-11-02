%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2021 8:46
%%%-------------------------------------------------------------------
-module(request_processor).
-author("alex").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([
  save_message/1,
  process_payload/1,
  find_message/1,
  delete_message/1,
  get_original_message/1]).

-include_lib("utils/include/mnesia_model.hrl").
-define(SERVER, ?MODULE).
-define(SERVER_REQUEST(Command, RecordName, Payload), {Command, RecordName, Payload}).

-define(SAVE_REQUEST(RecordName, Payload), ?SERVER_REQUEST(save, RecordName, Payload)).
-define(FIND_REQUEST(RecordName, Payload), ?SERVER_REQUEST(find, RecordName, Payload)).
-define(DELETE_REQUEST(RecordName, Payload), ?SERVER_REQUEST(delete, RecordName, Payload)).

-define(SAVE_MESSAGE(RecordName, Payload), ?SAVE_REQUEST(message, Payload)).
-define(FIND_MESSAGE(RecordName, Payload), ?FIND_REQUEST(message, Payload)).
-define(DELETE_MESSAGE(RecordName, Payload), ?DELETE_REQUEST(message, Payload)).


-record(request_processor_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #request_processor_state{}} | {ok, State :: #request_processor_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #request_processor_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #request_processor_state{}) ->
  {reply, Reply :: term(), NewState :: #request_processor_state{}} |
  {reply, Reply :: term(), NewState :: #request_processor_state{}, timeout() | hibernate} |
  {noreply, NewState :: #request_processor_state{}} |
  {noreply, NewState :: #request_processor_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #request_processor_state{}} |
  {stop, Reason :: term(), NewState :: #request_processor_state{}}).
handle_call({save, AvroBinary}, _From, State = #request_processor_state{}) ->
  UUID = uuid:get_v4(),
  Message = #message{
    correlation_id = UUID,
    payload = AvroBinary,
    status = created
  },
  {reply, crud_db_server:call_save(Message), State};


handle_call(?SERVER_REQUEST(find, AvroRecordName, UUID), _From, State = #request_processor_state{}) ->
  Message = crud_db_server:find(AvroRecordName, UUID),
  case Message of
    #message{correlation_id = UUID} ->
      {reply, Message, State};
    _NotFound ->
      {reply, ?NOT_FOUND, State}
  end;

handle_call(?SERVER_REQUEST(delete, AvroRecordName, UUID), _From, State = #request_processor_state{}) ->
  Deleted = crud_db_server:delete(AvroRecordName, UUID),
  case Deleted of
    UUID -> {reply, ok, State};
    _Error -> {reply, error, State}
  end;

handle_call(_Request, _From, State = #request_processor_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #request_processor_state{}) ->
  {noreply, NewState :: #request_processor_state{}} |
  {noreply, NewState :: #request_processor_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #request_processor_state{}}).
handle_cast(_Request, State = #request_processor_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #request_processor_state{}) ->
  {noreply, NewState :: #request_processor_state{}} |
  {noreply, NewState :: #request_processor_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #request_processor_state{}}).
handle_info(_Info, State = #request_processor_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #request_processor_state{}) -> term()).
terminate(_Reason, _State = #request_processor_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #request_processor_state{},
    Extra :: term()) ->
  {ok, NewState :: #request_processor_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #request_processor_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Save the JSON payload as AVRO binary.
%%% return {message, correlation_id, status, payload = AvroBinaryFormat}
%%%===================================================================
-spec(save_message(ServerName :: atom(), JsonAsBinary :: binary())
      -> RecordData :: tuple() | ?INVALID_SERVICE_PAYLOAD | ?BAD_REQUEST | ?SERVER_IS_NOT_RUNNING).
save_message(ServerName, JsonAsBinary) when is_atom(ServerName) andalso is_binary(JsonAsBinary) ->

  save_message(ServerName, JsonAsBinary, ?IS_PROCESS_ALIVE(ServerName));

save_message(_ServerName, _JsonAsBinary) ->
  ?BAD_REQUEST.

-spec(save_message(AvroBinary :: binary())
      -> RecordData :: tuple() | ?INVALID_SERVICE_PAYLOAD | ?BAD_REQUEST | ?SERVER_IS_NOT_RUNNING).
save_message(AvroBinary) ->
  save_message(?SERVER, AvroBinary).

save_message(ServerName, AvroBinary, true) ->
  gen_server:call(ServerName, {save, AvroBinary});

save_message(_ServerName, _JsonAsBinary, false) ->
  ?SERVER_IS_NOT_RUNNING.

%%%===================================================================
%%% Search for saved message by ID(UUID string)
%%% return {message, correlation_id = UUID, status, payload = AvroBinaryFormat}
%%%===================================================================

find_message(CorrelationUUID) ->
  find_message(?SERVER, CorrelationUUID).

find_message(ServerName, CorrelationUUID) ->
  find_message(ServerName, CorrelationUUID, ?IS_PROCESS_ALIVE(ServerName)).

-spec(find_message(ServerName :: atom(), CorrelationUUID :: string())
      -> RecordData :: tuple()).
find_message(ServerName, CorrelationUUID, true)
  when is_atom(ServerName) andalso is_binary(CorrelationUUID) ->
  gen_server:call(ServerName, ?SERVER_REQUEST(find, message, CorrelationUUID));

find_message(_ServerName, _CorrelationUUID, true) -> ?INVALID_FUNCTION_CALL;

find_message(_ServerName, _CorrelationUUID, false) ->
  ?SERVER_IS_NOT_RUNNING.


get_original_message(UUID) when is_binary(UUID) ->
  case to_uuid(UUID) of
    undefined ->
      {?INVALID, <<"\"message\": \"invalid path varaible: uuid.v4 is expected\"">>};
    BinaryID when is_binary(BinaryID) ->
      case find_message(BinaryID) of
        M when is_record(M, message) ->
          {ok, elavro_srv:avro_to_json(message, M#message.payload)};
        ?INVALID_FUNCTION_CALL ->
          {?INVALID_FUNCTION_CALL, <<"{\"message\":\"internal.function.call.failure\"}">>};
        ?NOT_FOUND ->
          {?NOT_FOUND, <<"{\"message\":\"data.not.found\"}">>};
        ?SERVER_IS_NOT_RUNNING ->
          {?SERVER_IS_NOT_RUNNING, <<"{\"message\":\"temporary.unavailable\"}">>};
        _Error ->
          {?ERROR, <<"{\"message\":\"server.error.\"}">>}
      end
  end;

get_original_message(_CorrelationUUID) ->
  lager:error("Binary is required."),
  {?INVALID_FUNCTION_CALL, <<"{\"message\":\"internal.function.call.failure\"}">>}.

to_uuid(UUID) ->
  try
    uuid:string_to_uuid(binary_to_list(UUID))
  catch
    E:R  ->
      lager:error("Failed to parse uuid. {~p, ~p}", [E, R]),
      undefined
  end.

%%%===================================================================
%%% Delete message by ID(UUID string)
%%% return ok|error
%%%===================================================================
-spec(delete_message(CorrelationUUID :: string())
      -> ok|error).
delete_message(CorrelationUUID) ->
  gen_server:call(?SERVER, ?SERVER_REQUEST(delete, message, CorrelationUUID)).

-spec(process_payload(binary()) ->
  M :: #message{}| {Status :: atom(), []} | {Status :: atom(), ErrorMessage :: binary()}).
process_payload(Binary) when is_binary(Binary) ->
  Response = payload_validator:process_json_to_avro(message, Binary),
  process_payload(Response);

process_payload({ok, AvroBinary}) when is_binary(AvroBinary) ->
  {ok, request_processor:save_message(AvroBinary)};

process_payload(R = {Status, []}) when is_atom(Status) ->
  R;

process_payload(R = {Status, ErrorMessage}) when is_atom(Status) andalso is_binary(ErrorMessage) ->
  R;

process_payload(_Err) ->
  {?INVALID_FUNCTION_CALL, []}.