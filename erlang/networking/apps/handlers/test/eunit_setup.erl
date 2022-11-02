%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jan 2021 12:52
%%%-------------------------------------------------------------------
-module(eunit_setup).
-author("alex").

%% API
-export([
  load_files/1,
  load_json_files/0,
  load_avro_files/0,
  generate_avro_files/1,
  setup/0,
  cleanup/1]).

-define(JSON_RESOURCES, "./apps/handlers/test/resources/json").
-define(AVRO_RESOURCES, "./apps/handlers/test/resources/avro").
-define(CONFIG_DIR, "./apps/handlers/resources").

load_files(DirName) ->
  lager:info("Current dir: ~p ",
    [file:get_cwd()]),
  {ok, FileNames} = file:list_dir(DirName),
  lists:map(
    fun(N) ->
      {ok, Binary} = file:read_file(DirName ++ "/" ++ N),
      [FileName, _] = string:tokens(N, "."),
      {FileName, Binary}
    end,
    FileNames).

load_json_files() ->
  load_files(?JSON_RESOURCES).

load_avro_files() ->
  load_files(?AVRO_RESOURCES).

setup() ->
  application:set_env(handlers, avro_schema_dir, ?CONFIG_DIR),
  application:ensure_all_started(handlers),
  JsonFiles = load_json_files(),
  [JsonFiles, []].

cleanup(_D) ->
  app_supervisor:terminate_child(avro_handler),
  application:stop(service_manager),
  application:stop(lager).

generate_avro_files(JsonFiles) ->
  lists:foreach(
    fun({FileName, C}) ->
      {TL} = jiffy:decode(C),
      Schema = elavro_srv:get_schema(message),
      Encoder = avro:make_simple_encoder(Schema, []),
      Encoded = iolist_to_binary(Encoder(TL)),
      file:write_file("./apps/handlers/test/resources/avro/" ++ FileName ++ ".ocf", Encoded)
    end,
    JsonFiles).