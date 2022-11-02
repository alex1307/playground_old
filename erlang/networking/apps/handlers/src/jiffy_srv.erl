%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jan 2021 18:08
%%%-------------------------------------------------------------------
-module(jiffy_srv).
-author("alex").

-behaviour(gen_server).
-define(DECODE_JSON(B), {from_json, B}).
-define(ENCODE_JSON(T), {to_json, T}).

-include_lib("utils/include/service_space.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([
  tuple_list_to_binary/2,
  tuple_list_to_binary/1]
).

-export([
  binary_to_tuple_list/2,
  binary_to_tuple_list/1]
).

-export([
  json_to_avro/2,
  json_to_avro/4
]).

-export([trim/2]).

-define(SERVER, ?MODULE).

-record(encoder_srv_state, {}).

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
  {ok, State :: #encoder_srv_state{}} | {ok, State :: #encoder_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #encoder_srv_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #encoder_srv_state{}) ->
  {reply, Reply :: term(), NewState :: #encoder_srv_state{}} |
  {reply, Reply :: term(), NewState :: #encoder_srv_state{}, timeout() | hibernate} |
  {noreply, NewState :: #encoder_srv_state{}} |
  {noreply, NewState :: #encoder_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #encoder_srv_state{}} |
  {stop, Reason :: term(), NewState :: #encoder_srv_state{}}).

handle_call(?DECODE_JSON(Bin), _From, State) ->
  try jiffy:decode(Bin) of
    {PropList} when is_list(PropList) ->
        {reply, normalize_property_list(PropList), State}
    catch
      error: Reason ->
        lager:error("Parsing error: {~p, ~p}", [error, Reason]),
        {reply, ?INVALID_JSON, State}
  end;

handle_call(?ENCODE_JSON(TupleList), _From, State) ->
  Binary = jiffy:encode({TupleList}),
  {reply, Binary, State};

handle_call(_Request, _From, State = #encoder_srv_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #encoder_srv_state{}) ->
  {noreply, NewState :: #encoder_srv_state{}} |
  {noreply, NewState :: #encoder_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #encoder_srv_state{}}).
handle_cast(_Request, State = #encoder_srv_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #encoder_srv_state{}) ->
  {noreply, NewState :: #encoder_srv_state{}} |
  {noreply, NewState :: #encoder_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #encoder_srv_state{}}).
handle_info(_Info, State = #encoder_srv_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #encoder_srv_state{}) -> term()).
terminate(_Reason, _State = #encoder_srv_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #encoder_srv_state{},
    Extra :: term()) ->
  {ok, NewState :: #encoder_srv_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #encoder_srv_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(
binary_to_tuple_list(ServerName :: atom(), Binary :: binary()) -> [A] | ?INVALID_SERVICE_PAYLOAD
  when A :: tuple()
).

binary_to_tuple_list(ServerName, Binary) when is_binary(Binary) andalso is_atom(ServerName) ->
  binary_to_tuple_list(ServerName, Binary, ?IS_PROCESS_ALIVE(ServerName));

binary_to_tuple_list(_ServerName, _Binary) ->
  ?INVALID_FUNCTION_CALL.

binary_to_tuple_list(ServerName, Binary, true) when is_binary(Binary) andalso is_atom(ServerName) ->
  gen_server:call(ServerName, ?DECODE_JSON(Binary));

binary_to_tuple_list(ServerName, Binary, false) when is_binary(Binary) andalso is_atom(ServerName) ->
  ?SERVER_IS_NOT_RUNNING.

trim([], Bin) ->
  Bin;

trim([{F,R}|T], Bin) ->
  trim(T, binary:replace(Bin, F, R, [global])).

normalize_property_list(PL)->
  lists:map(fun({K, V}) -> {K, normalize(V)} end, PL).

normalize([{V}]) ->
  [V];
normalize([{H} | T]) ->
  lists:append([H], normalize(T));
normalize({V}) -> V;
normalize(V) -> V.

-spec(
binary_to_tuple_list(Binary :: binary()) -> [A]
  when A :: {term(), term()}
).
binary_to_tuple_list(Binary) ->
  binary_to_tuple_list(?SERVER, Binary).

-spec(
tuple_list_to_binary(ServerName :: atom(), TupleList :: [A]) -> binary()
  when A :: {term(), term()}
).
tuple_list_to_binary(ServerName, TupleList) when is_atom(ServerName) andalso is_list(TupleList) ->
  gen_server:call(ServerName, ?ENCODE_JSON(TupleList));

tuple_list_to_binary(_ServerName, _TupleList) ->
  invalid_parameters.

-spec(
tuple_list_to_binary(TupleList :: [A]) -> binary()
  when A :: {term(), term()}
).
tuple_list_to_binary(TupleList) ->
  tuple_list_to_binary(?SERVER, TupleList).

-spec(json_to_avro(JsonSrv::atom(),
    AvroSrv::atom(),
    AvroRecordName::atom(),
    Binary::binary()) -> binary()| ? BAD_REQUEST).
json_to_avro(JsonSrv, AvroSrv, AvroRecordName, Binary)
  when is_atom(JsonSrv) andalso is_atom(AvroSrv) andalso is_atom(AvroRecordName) andalso is_binary(Binary)->
  Resp = binary_to_tuple_list(JsonSrv, Binary),
  lager:info("Jiffy parsing service: ~p", [Resp]),
  case Resp of
    ?SERVER_IS_NOT_RUNNING -> ?SERVER_IS_NOT_RUNNING;
    ?INVALID_SERVICE_PAYLOAD -> ?INVALID_SERVICE_PAYLOAD;
    PropList when is_list(PropList) ->
      elavro_srv:encode(AvroSrv, AvroRecordName, PropList)
  end;

json_to_avro(_JsonSrv, _AvroSrv, _AvroRecordName, _Binary) ->
  ?BAD_REQUEST.

-spec(json_to_avro(
    AvroRecordName::atom(),
    Binary::binary()) -> binary()| ?INVALID_SERVICE_PAYLOAD).
json_to_avro(AvroRecordName, Binary) when is_atom(AvroRecordName) andalso is_binary(Binary)->
  case binary_to_tuple_list(Binary) of
    ?INVALID_SERVICE_PAYLOAD ->
      ?INVALID_SERVICE_PAYLOAD;
    PropList when is_list(PropList) ->
      elavro_srv:encode(AvroRecordName, PropList)
  end.