
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 24-Aug-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(crud_eunit).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").
-include_lib("utils/include/mnesia_model.hrl").
-include_lib("eunit/include/eunit.hrl").

ensure_app_supervisor_is_started_test() ->
  application:ensure_all_started(nosql_db_service),
  application:set_env(mnesia, dir, "mnesia-db"),
  application:start(mnesia),
  app_supervisor:start_child(crud, crud_db_server, start_link, []),
  Pid = whereis(app_supervisor),
  ?assert(is_pid(Pid)),
  ?assert(is_pid(whereis(crud_db_server))).

ensure_crud_test() ->
  application:ensure_all_started(lager),
  application:ensure_all_started(mnesia_app),
  ?assert(is_pid(whereis(crud_db_server))),
  UUID = common_utils:get_uuid(),
  Message = #message{correlation_id = UUID,
    type = message,
    payload = <<"SUCCESS">>,
    status = success},
  Message = crud_db_server:call_save(Message),
  Response = crud_db_server:find(message, UUID),
  ?assert(Message == Response),
  ?assert(not_found == crud_db_server:find(message, <<"UUID">>)),
  crud_db_server:delete(message, UUID),
  ?assert(not_found == crud_db_server:find(message, UUID)).
