
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 10-Dec-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(elavro_srv).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-behaviour(gen_server).

%% API

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([start_link/1, start_link/2]).
-export([get_records/0, get_records/1]).
-export([get_schema/2, get_schema/1]).
-export([get_dir/0, get_dir/1]).
-export([get_status/0, get_status/1]).

-export([encode/3, encode/2]).
-export([decode/3, decode/2]).
-export([avro_to_json/4, avro_to_json/2]).

-define(SERVER, ?MODULE).
-define(ENCODE(R, P), {encode, R, P}).
-define(DECODE(R, P), {decode, R, P}).

-include("model.hrl").
-include_lib("utils/include/service_space.hrl").
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Dir :: list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Dir) ->
  start_link(?SERVER, Dir).

-spec(start_link(ServerName :: atom(), Dir :: iolist()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ServerName, Dir) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [Dir], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #avro_handler_state{}} | {ok, State :: #avro_handler_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Dir]) ->
  State = handlers_utils:load_resources(Dir),
  {ok, State}.


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: {atom(), list()} | {atom(), binary()}, From :: {pid(), Tag :: term()},
    State :: #avro_handler_state{}) ->
  {reply, Reply :: term(), NewState :: #avro_handler_state{}} |
  {reply, Reply :: term(), NewState :: #avro_handler_state{}, timeout() | hibernate} |
  {noreply, NewState :: #avro_handler_state{}} |
  {noreply, NewState :: #avro_handler_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #avro_handler_state{}} |
  {stop, Reason :: term(), NewState :: #avro_handler_state{}}).

handle_call(status, _From, State = #avro_handler_state{status = Status}) ->
  {reply, Status, State};

handle_call(dir, _From, State = #avro_handler_state{status = ok, schema_dir = Dir}) ->
  {reply, Dir, State};

handle_call(records, _From, State = #avro_handler_state{status = ok, schemas = Schemas}) ->
  maps:keys(Schemas),
  {reply, maps:keys(Schemas), State};

handle_call({schema, SchemaName}, _From, State = #avro_handler_state{status = ok, schemas = Schemas}) ->
  {reply, maps:get(SchemaName, Schemas), State};

handle_call(?ENCODE(RecordName, Payload), _From, State = #avro_handler_state{status = ok, encoders = Encoders}) ->
  IS_KEY = maps:is_key(RecordName, Encoders),
  if
    IS_KEY ->
      Encoder = maps:get(RecordName, Encoders),
      try Encoder(Payload) of
        IOList when is_list(IOList) ->
          {reply, iolist_to_binary(IOList), State}
        catch
          error:Reason ->
            lager:error("Avro parsing error: ~p", [Reason]),
            {reply, ?INVALID_AVRO, State}
      end;
    true ->
      {reply, ?RECORD_NOT_FOUND, State}
  end;

handle_call(?DECODE(RecordName, Payload), _From, State = #avro_handler_state{status = ok, decoders = Decoders}) ->
  IS_KEY = maps:is_key(RecordName, Decoders),
  if
    IS_KEY ->
      Decoder = maps:get(RecordName, Decoders),
      {reply, Decoder(Payload), State};
    true ->
      {reply, record_not_found, State}
  end;

handle_call(_Request, _From, State = #avro_handler_state{status = Nok}) when Nok =/= ok ->
  {reply, Nok, State};

handle_call(_Request, _From, State = #avro_handler_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #avro_handler_state{}) ->
  {noreply, NewState :: #avro_handler_state{}} |
  {noreply, NewState :: #avro_handler_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #avro_handler_state{}}).
handle_cast(_Request, State = #avro_handler_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #avro_handler_state{}) ->
  {noreply, NewState :: #avro_handler_state{}} |
  {noreply, NewState :: #avro_handler_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #avro_handler_state{}}).
handle_info(_Info, State = #avro_handler_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #avro_handler_state{}) -> term()).
terminate(_Reason, _State = #avro_handler_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #avro_handler_state{},
    Extra :: term()) ->
  {ok, NewState :: #avro_handler_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #avro_handler_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(decode(ServerName :: atom(), Record :: atom(), Binary :: binary()) -> [A] | invalid_function_call | record_not_found
  when A :: tuple()).
decode(ServerName, Record, Binary)
  when is_atom(ServerName) andalso is_atom(Record) andalso is_binary(Binary) ->
  gen_server:call(ServerName, ?DECODE(Record, Binary));

decode(_ServerName, _Rec, _Bin) ->
  ?INVALID_FUNCTION_CALL.

-spec(decode(Record :: atom(), Term :: binary()) -> [A] | invalid_function_call | record_not_found
  when A :: tuple()).
decode(Record, Binary) ->
  decode(?SERVER, Record, Binary).


-spec(encode(ServerName :: atom(), Record :: atom(), TupleList :: [A]) -> binary() | invalid_function_call | record_not_found
  when A :: tuple()).
encode(ServerName, Record, TupleList) ->
  encode(ServerName, Record, TupleList, ?IS_PROCESS_ALIVE(ServerName)).

encode(ServerName, Record, TupleList, true)
  when is_atom(ServerName) andalso is_atom(Record) andalso is_list(TupleList) ->
  gen_server:call(ServerName, ?ENCODE(Record, TupleList));

encode(_ServerName, _Record, _TupleList, true) ->
  ?INVALID_FUNCTION_CALL;

encode(ServerName, _Record, _TupleList, false)
  when is_atom(ServerName) ->
  lager:error("Server ~p is not running", [ServerName]),
  ?SERVER_IS_NOT_RUNNING.


-spec(encode(Record :: atom(), Term :: binary()) -> [A] | invalid_function_call | record_not_found
  when A :: tuple()).
encode(Record, TupleList) ->
  encode(?SERVER, Record, TupleList).

-spec(avro_to_json(AvroSrv :: atom(), JsonSrv :: atom(), RecordName :: atom(), Binary :: binary()) -> binary() | invalid_parameters).
avro_to_json(AvroSrv, JsonSrv, RecordName, Binary)
  when is_atom(AvroSrv) andalso is_atom(JsonSrv) andalso is_atom(RecordName) andalso is_binary(Binary) ->
  TupleList = encode(AvroSrv, RecordName, Binary),
  jiffy_srv:tuple_list_to_binary(JsonSrv, TupleList);

avro_to_json(_AvroSrv, _JsonSrv, _RecordName, _Binary) ->
  ?INVALID_FUNCTION_CALL.

-spec(avro_to_json(RecordName :: atom(), Binary :: binary()) -> binary() | invalid_parameters).
avro_to_json(RecordName, Binary)
  when is_atom(RecordName) andalso is_binary(Binary) ->
  TupleList = decode(RecordName, Binary),
  jiffy_srv:tuple_list_to_binary(TupleList).


get_status(ServerName) ->
  gen_server:call(ServerName, status).

get_status() ->
  get_status(?SERVER).

get_dir(ServerName) ->
  gen_server:call(ServerName, dir).

get_dir() ->
  get_dir(?SERVER).

get_records(ServerName) ->
  gen_server:call(ServerName, records).

get_records() ->
  get_records(?SERVER).

get_schema(ServerName, RecordName) ->
  gen_server:call(ServerName, {schema, RecordName}).

get_schema(RecordName) ->
  get_schema(?SERVER, RecordName).
