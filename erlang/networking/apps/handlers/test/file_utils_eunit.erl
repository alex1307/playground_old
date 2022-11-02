
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 24-Aug-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(file_utils_eunit).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-include_lib("eunit/include/eunit.hrl").


ensure_dir_test() ->
  OK = file_utils:ensure_dir("./apps/handlers/resources"),
  ?assert(OK == ok).


ensure_file_test() ->
  application:ensure_all_started(service_manager),
  application:ensure_all_started(handlers),
  application:ensure_all_started(lager),
  {ok, ConfigDir} = application:get_env(handlers, avro_schema_dir),
%%  lager:info("Avro dir: ~p", [ConfigDir]),
  {ok, _IoDev} = file:open("./apps/handlers/resources/message.avsc", read),
  ok = file_utils:ensure_dir(ConfigDir),
  ?assert(ConfigDir ++ "/message.avsc" == "./apps/handlers/resources/message.avsc"),
  {ok, FileNames} = file:list_dir(ConfigDir),
  ?assert(length(FileNames) == 1),
  [F|_] = FileNames,
  ?assert(F == "message.avsc").

files_to_map_test() ->
  application:ensure_all_started(handlers),
  application:ensure_all_started(lager),
  {ok, ConfigDir} = application:get_env(handlers, avro_schema_dir),
  Map = file_utils:map_dir(ConfigDir),
%%  lager:info("Map: ~p", [Map]),
%%  lager:info("Size: ~p", maps:size(Map)),
  ?assert(1 == maps:size(Map)),
  ?assert(maps:get(message, Map) == "message.avsc").
