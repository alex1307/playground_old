
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 21-Aug-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(file_utils).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-include_lib("kernel/include/file.hrl").

%% API

-export([
  ensure_dir/1,
  ensure_file/1,
  delete_dir_recursively/1,
  file_size/1,
  map_dir/1,
  map_dir_to_file_content/1]).

ensure_dir(Dir) ->
  case filelib:is_dir(Dir) of
    false ->
      file:make_dir(Dir);
    true ->
      ok
  end.

ensure_file(FileName) ->
  case file:read_file_info(FileName) of
    {ok, FI} ->
      lager:info("File ~p. Info: ~p", [FileName, FI]),
      ok;
    _Error ->
      {ok, IO_Device} = file:open(FileName, write),
      {ok, Info} = file:read_file_info(IO_Device),
      lager:info("File ~p. Info: ~p", [FileName, Info]),
      file:close(IO_Device),
      ok
  end.

delete_dir_recursively(Dir) ->
  case filelib:is_dir(Dir) of
    true ->
      file:del_dir_r(Dir);
    _F ->
      lager:info("Dir ~p does not exist", [Dir]),
      ok
  end.

file_size(FileName) ->
  case filelib:is_file(FileName) of
    true ->
      {ok, Info} = file:read_file_info(FileName),
      Info#file_info.size;
    false ->
      error
  end.

-spec(map_dir(Dir::iolist()) -> M when
  M :: #{Key => Value},
  Key::atom(),
  Value::iolist()).
map_dir(Dir) when is_list(Dir) ->
  True = filelib:is_dir(Dir),
  lager:info("is dir ~p: ~p", [Dir, True]),
  if
    True  ->
      map_files(file:list_dir(Dir));
    true ->
      error
  end.

map_dir_to_file_content(Dir) ->
  True = filelib:is_dir(Dir),
  if
    True  ->
      {ok, FileNames} = file:list_dir(Dir),
      map_file_content(Dir, FileNames, ["avsc"]);
    true ->
      not_dir
  end.

map_files(P = {ok, FileNames}) when is_list(FileNames)->
  map_files(P, ["avsc"]).

-spec(map_files(T, E) -> map()|error when
  T::{ok, list()},
  E::list()
).
map_files({ok, FileNames}, FileExt) when is_list(FileNames) andalso is_list(FileExt) ->
  Filtered = lists:filter(
    fun(N) ->
      [_|T] = string:tokens(N, "."),
      case T of
        [Ext] ->
          lists:any(
            fun(E) ->
              E == Ext
              end,
            FileExt);
        _E ->
          false
      end
    end,
    FileNames),

  TupleList = lists:map(
    fun(N) ->
      [FileName|_] = string:tokens(N, "."),
      {list_to_atom(FileName), N}
    end ,
    Filtered),

  maps:from_list(TupleList);

map_files(_, _) -> error.

map_file_content(Dir, FileNames, FileExt)
  when is_list(FileNames) andalso is_list(FileExt) andalso is_list(Dir)->
  Filtered = lists:filter(
    fun(N) ->
      [_|T] = string:tokens(N, "."),
      case T of
        [Ext] ->
          lists:any(
            fun(E) ->
              E == Ext
            end,
            FileExt);
        _E ->
          false
      end
    end,
    FileNames),

  TupleList = lists:map(
    fun(N) ->
      [FileName|_] = string:tokens(N, "."),
      {ok, Binary} = file:read_file(Dir ++ "/" ++ N),
      {list_to_atom(FileName), Binary}
    end ,
    Filtered),
  maps:from_list(TupleList);

map_file_content(_, _, _) ->
  error.


