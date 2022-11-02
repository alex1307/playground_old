%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jan 2016 10:30 AM
%%%-------------------------------------------------------------------
-module(crud_db_server).
-author("alex").

-behaviour(gen_server).


%% API

-define(SERVER, ?MODULE).

-record(state, {}).
-define(PERSIST(Record), {save, Record}).
-define(DELETE(TableName, ID), {delete, TableName, ID}).
-define(FIND_BY_ID(TableName, ID), {find, TableName, ID}).


%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([start_link/0,start_link/1]).
-export([call_save/1, call_save/2]).
-export([cast_save/1, cast_save/2]).
-export([find/2, find/3]).
-export([delete/2, delete/3]).
-export([cast_delete/2, cast_delete/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(ServerName) when is_atom(ServerName)->
  gen_server:start_link({local, ServerName}, ?MODULE, [], []);

start_link(_ServerName) ->
  {error, invalid_server_name}.

init([]) ->
  process_flag(trap_exit, true),
  mnesia_api:create(),
  {ok, #state{}}.


handle_call(?PERSIST(Entity), _From, State) ->
%%  lager:info("Entity: ~p", [Entity]),
  case mnesia_api:save_or_update(Entity) of
    {atomic, ok} ->
      {reply, Entity, State};
    Err ->
      lager:info("Err: ~p", [Err]),
      {reply, error, State}
  end;


handle_call(?DELETE(TableName, ID), _From, State) ->
  case mnesia_api:delete(TableName, ID) of
    {atomic, ok} ->
%%      lager:info("Deleted: ~p", [ID]),
      {reply, ID, State};
    _Err ->
      {reply, error, State}
  end;


handle_call(?FIND_BY_ID(Table, ID), _From, State) ->
  case mnesia_api:get_by_id(Table, ID) of
    {atomic, [Found]} ->
      {reply, Found, State};
    _NotFound ->
      {reply, not_found, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(?DELETE(TableName, ID), State) ->
  mnesia_api:delete(TableName, ID),
  {noreply, State};

handle_cast(?PERSIST(Entity), State) ->
  mnesia_api:save_or_update(Entity),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(Any, State) ->
  lager:error("Unsupported message has been received: ~p", [Any]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


call_save(Data) when is_tuple(Data)->
  gen_server:call(?SERVER, ?PERSIST(Data));

call_save(_Data) ->
  invalid_tuple.


call_save(ServerName, Data) when is_tuple(Data) ->
  gen_server:call(ServerName, ?PERSIST(Data));

call_save(_, _)  ->
  invalid_tuple.


cast_save(Data) when is_tuple(Data)->
  gen_server:cast(?SERVER, ?PERSIST(Data));

cast_save(_Data) ->
  invalid_tuple.


cast_save(ServerName, Data) when is_tuple(Data) ->
  gen_server:cast(ServerName, ?PERSIST(Data));

cast_save(_ServerName, _Data) ->
  invalid_tuple.
-spec(find(TableName::atom(), ID::term()) -> FoundEntity::term()|not_found).
find(TableName, ID) when is_atom(TableName) ->
  find(?SERVER, TableName, ID).

find(ServerName, TableName, ID) when is_atom(TableName) ->
  gen_server:call(ServerName, ?FIND_BY_ID(TableName, ID)).

delete(ServerName, TableName, ID) ->
  gen_server:call(ServerName, ?DELETE(TableName, ID)).

delete(TableName, ID) ->
  delete(?SERVER, TableName, ID).

cast_delete(TableName, ID) ->
  cast_delete(?SERVER, TableName, ID).

cast_delete(ServerName, TableName, ID) ->
  gen_server:cast(ServerName, ?DELETE(TableName, ID)).