%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2017 3:07 PM
%%%-------------------------------------------------------------------
-module(mnesia_utils).
-author("alex").
-include("mnesia_model.hrl").

-compile(export_all).

get_index(L, Member) ->
  get_index(L, Member, 1).

get_index([], _, _Index) -> not_found;

get_index([Member | _T], Member, Index) ->
  Index;

get_index([_H | T], Member, Index) ->
  get_index(T, Member, Index + 1).


match_record(Record, PropValues)
  when
  is_tuple(Record) andalso
  is_list(PropValues)->

  lager:debug("Records = ~p, Index = ~p", [Record,PropValues]),

  RecordName = element(1, Record),
  Fields = ?GET_FIELDS(RecordName),

  lager:debug("Record name = ~p, Fields = ~p", [RecordName,Fields]),

  lists:all(
    fun({Key, Value}) ->

      Idx = get_index(Fields, Key),
      Data = element(Idx + 1, Record),
      Value == Data

    end,
    PropValues);

match_record(Record,PropValues) ->
  lager:debug("Records = ~p, Index = ~p", [Record,PropValues]),
  false.
