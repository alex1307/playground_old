
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 24-Aug-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(handlers_utils_eunit).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").
-include_lib("eunit/include/eunit.hrl").
-include("model.hrl").


file_name_test() ->
  F1 = "message.avsc",
  F2 = "message",
  F3 = "message.txt",
  F4 = "message.avsc.orig",
  ?assert(handlers_utils:file_name(F4) == nok),
  ?assert(handlers_utils:file_name(F3) == nok),
  ?assert(handlers_utils:file_name(F2) == nok),
  ?assert(handlers_utils:file_name(F1) == "message").

filter_test() ->
  application:ensure_all_started(service_manager),
  application:ensure_all_started(handlers),
  application:ensure_all_started(lager),
  {ok, ConfigDir} = application:get_env(handlers, avro_schema_dir),
  Files = ["message.avsc", "message", "message.txt", "message.avsc.orig", [], file],
  Map = handlers_utils:map_dir_to_file_content(ConfigDir),
  Result = handlers_utils:filter_and_map_files(ConfigDir, Files),
  ?assert(Result == Map).

map_dir_to_file_content_test() ->
  application:ensure_all_started(handlers),
  application:ensure_all_started(lager),
  {ok, ConfigDir} = application:get_env(handlers, avro_schema_dir),
  Map = handlers_utils:map_dir_to_file_content(ConfigDir),
  ?assert(maps:size(Map) == 1),
  Schema = maps:get(message, Map),
  ?assert(is_binary(Schema)).

load_resources_test() ->
  application:ensure_all_started(handlers),
  application:ensure_all_started(lager),
  {ok, ConfigDir} = application:get_env(handlers, avro_schema_dir),
  State = handlers_utils:load_resources(ConfigDir),
  ?assert(State#avro_handler_state.status == ok).
