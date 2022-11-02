%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2016 10:40 AM
%%%-------------------------------------------------------------------
-module(mnesia_api).
-author("alex").

-include_lib("utils/include/mnesia_model.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% API
-export([create/0,
  drop/0,
  save_or_update/1,
  get_by_id/2,
  delete/2,
  query/1,
  query_by_index/2]).

create() ->
  lager:info("Mnesia nodes: ~p ~n", [erlang:node()]),
  mnesia:create_schema(erlang:node()),
  load_schema(?MNESIA_TABLES).

drop() ->
  lists:map(fun(TableName) -> mnesia:delete_table(TableName) end, ?MNESIA_TABLES).

%add_node(Node)->
%  mnesia:change_config(extra_db_nodes, Node).

create_table(TableName) ->
  lager:info("Create table ~p ~n Properties: ~p ~n", [TableName, ?GET_PROPS(TableName)]),
  mnesia:create_table(TableName, ?GET_PROPS(TableName)).


load_table(TableName) ->
  try
    mnesia:table_info(TableName, record_name)
  catch exit:_ ->
    create_table(TableName)
  end.

load_schema(Tables) ->
  lists:map(fun(TableName) -> load_table(TableName) end, Tables),
  mnesia:wait_for_tables(?MNESIA_TABLES, ?PT_TIMEOUT).


%%%%%%%%%%%%%%%%%
% BASIC CRUD OPERATIONS
%%%%%%%%%%%%%%%%%

save_or_update(Obj) ->
  mnesia:transaction(fun() -> mnesia:write(Obj) end).

delete(Table, Id) ->
  mnesia:transaction(fun() -> mnesia:delete({Table, Id}) end).

get_by_id(Table, Id) ->
  mnesia:transaction(fun() -> mnesia:read({Table, Id}) end).

query(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

query_by_index(TableName, PropValues) ->
  Res =
    query(
      qlc:q(
        [T ||
          T <- mnesia:table(TableName),
          mnesia_utils:match_record(T, PropValues)
        ])
    ),
  Res.



