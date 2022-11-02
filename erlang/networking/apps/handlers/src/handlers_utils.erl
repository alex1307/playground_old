
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 16-Dec-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(handlers_utils).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").
-include("model.hrl").
-include("constants.hrl").
%% API


-ifdef(TEST).
-compile(export_all).
-endif.

-ifndef(TEST).
-export([load_resources/1]).
-endif.


-spec(load_resources(Dir :: iolist()) -> State when State :: #avro_handler_state{}).
load_resources(Dir) ->
  Schema = map_dir_to_file_content(Dir),
  State = load(Schema),
  State#avro_handler_state{
    schema_dir = Dir
  }.

-spec(load(Map :: map()|not_dir | error) -> #avro_handler_state{}).
load(not_dir) ->
  #avro_handler_state{
    status = dir_not_found
  };

load(error) ->
  #avro_handler_state{
    status = processing_error
  };

load(Schemas) when is_map(Schemas) ->
  Keys = maps:keys(Schemas),
  EncoderList = lists:map(
    fun(K) ->
      Schema = maps:get(K, Schemas),
      {K, avro:make_simple_encoder(Schema, [])}
    end,
    Keys),
  DecoderList = lists:map(
    fun(K) ->
      Schema = maps:get(K, Schemas),
      {K, avro:make_simple_decoder(Schema, [])}
    end,
    Keys),
  #avro_handler_state{
    status = ok,
    schemas = Schemas,
    encoders = maps:from_list(EncoderList),
    decoders = maps:from_list(DecoderList)
  };

load(_) ->
  #avro_handler_state{
    status = error
  }.

map_dir_to_file_content(Dir) ->
  True = filelib:is_dir(Dir),
  if
    True ->
      map_file_content(Dir, file:list_dir(Dir));
    true ->
      not_dir
  end.

map_file_content(_Dir, {error, _R}) ->
  error;

map_file_content(Dir, {ok, FileNames})
  when is_list(FileNames) andalso is_list(Dir) ->
  filter_and_map_files(Dir, FileNames);

map_file_content(_, _) -> error.


filter_and_map_files(Dir, Files) ->
  TupleList = lists:map(
    fun(N) ->
      case file_name(N) of
        error ->
          nok;
        nok ->
          nok;
        FN ->
          {ok, Binary} = file:read_file(Dir ++ "/" ++ N),
          {list_to_atom(FN), Binary}
      end
    end,
    Files),
  maps:from_list(lists:filter(fun filter/1, TupleList)).


filter({_K, _V}) ->
  true;
filter(_) ->
  false.

file_name(FileName) when is_list(FileName) ->
  case string:tokens(FileName, ".") of
    [] ->
      nok;
    [N, ?AVRO_FILE_EXTENSION] ->
      N;
    L when is_list(L) ->
      nok
  end;

file_name(_) -> error.