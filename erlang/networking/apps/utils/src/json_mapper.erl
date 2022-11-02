%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2016 8:53 AM
%%%-------------------------------------------------------------------
-module(json_mapper).
-author("alex").

%% API
-compile(export_all).
-define(ERROR, error).



map_tuple2record(PropList, Fields, RecordName)
  when is_atom(RecordName) andalso is_list(PropList) ->
  list_to_tuple([RecordName|[proplists:get_value(X, PropList) || X <- Fields]]);

map_tuple2record(_PropList, _Fields, _RecordName) ->
  ?ERROR.

binary2record(Bin, Fields, RecordName) when is_binary(Bin) ->
  try
      {PropList} = jiffy:decode(Bin),
      map_tuple2record(PropList, Fields, RecordName)
  catch
    E:R ->
      lager:error("Unable to parse ~p
      Error: ~p
      Reason: ~p", [Bin, E, R]),
      ?ERROR
  end.

parse_list(TupleList, Fields, RecordName)->
  TupleToRecFun = fun map_tuple2record/3,
  MapFun = fun({Bin}) -> TupleToRecFun(Bin, Fields, RecordName) end,
  lists:map(MapFun, TupleList).
