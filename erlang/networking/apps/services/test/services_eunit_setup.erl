%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2021 11:52
%%%-------------------------------------------------------------------
-module(services_eunit_setup).
-author("alex").

%% API
-export([
  setup/0,
  cleanup/1
]).

-define(CONFIG_DIR, "./apps/handlers/resources").

setup() ->
  application:ensure_all_started(service_manager),
  application:ensure_all_started(lager),
  application:set_env(mnesia, dir, "mnesia-db"),
  application:start(mnesia),
  app_supervisor:start_child(elavro_srv, elavro_srv, start_link, [?CONFIG_DIR]),
  app_supervisor:start_child(jiffy_srv, jiffy_srv, start_link, []),
  app_supervisor:start_child(crud, crud_db_server, start_link, []),
  app_supervisor:start_child(request_processor, request_processor, start_link, []).

cleanup(_ServerName) ->
  app_supervisor:terminate_child(crud_db_server),
  app_supervisor:terminate_child(request_processor),
  application:stop(mnesia),
  application:stop(lager).