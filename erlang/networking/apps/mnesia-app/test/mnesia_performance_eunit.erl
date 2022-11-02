%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jan 2021 8:14
%%%-------------------------------------------------------------------
-module(mnesia_performance_eunit).
-author("alex").

-include_lib("eunit/include/eunit.hrl").
-include_lib("utils/include/mnesia_model.hrl").

setup() ->
  application:ensure_all_started(nosql_db_service),
  application:set_env(mnesia, dir, "mnesia-db"),
  application:start(mnesia),
  app_supervisor:start_child(crud, crud_db_server, start_link, []),
  app_supervisor:start_child(crud_db_server1, crud_db_server, start_link, [crud_db_server1]),
  app_supervisor:start_child(crud_db_server2, crud_db_server, start_link, [crud_db_server2]),
  app_supervisor:start_child(crud_db_server3, crud_db_server, start_link, [crud_db_server3]),
  app_supervisor:start_child(crud_db_server4, crud_db_server, start_link, [crud_db_server4]),
  app_supervisor:start_child(crud_db_server5, crud_db_server, start_link, [crud_db_server5]),

  ?_assert(is_process_alive(whereis(crud_db_server))),
  ?_assert(is_process_alive(whereis(crud_db_server1))),
  ?_assert(is_process_alive(whereis(crud_db_server2))),
  ?_assert(is_process_alive(whereis(crud_db_server3))),
  ?_assert(is_process_alive(whereis(crud_db_server4))),
  ?_assert(is_process_alive(whereis(crud_db_server5))),
  crud_db_server.

cleanup(ServerName) ->
  app_supervisor:terminate_child(ServerName),
  application:stop(mnesia),
  application:stop(lager).


standalone_performance_crud_test_(ServerName) ->
  lager:info("Memory: ~p", [erlang:memory()]),
  lager:info("Standalone CRUD: ~p", [timer:tc(fun apply_N_times/3, [ServerName, fun crud/2,100000])]),
  lager:info("Memory: ~p", [erlang:memory()]),
  ?_assert(is_process_alive(whereis(ServerName))).

standalone_performance_save_test_(ServerName) ->
  lager:info("Memory: ~p", [erlang:memory()]),
  lager:info("Standalone save: ~p", [timer:tc(fun apply_N_times/3, [ServerName, fun save_message/2,100000])]),
  lager:info("Standalone process: ~p", [erlang:process_info(whereis(crud_db_server))]),
  lager:info("Memory: ~p", [erlang:memory()]),
  ?_assert(is_process_alive(whereis(ServerName))).


lb_performance_crud_test_(ServerName) ->
  lager:info("Loadbalancer CRUD: ~p", [timer:tc(fun lb_apply_N_times/3, [ServerName, fun crud/2, 100000])]),

  ?_assert(is_process_alive(whereis(ServerName))).

lb_performance_save_test_(ServerName) ->
  lager:info("Loadbalancer save: ~p", [timer:tc(fun lb_apply_N_times/3, [ServerName, fun save_message/2, 100000])]),
  ?_assert(is_process_alive(whereis(ServerName))).


apply_N_times(_ServerName, _F, 0) ->
  ok;

apply_N_times(ServerName, F, N) ->
  F(ServerName, N),
  apply_N_times(ServerName, F, N - 1).


lb_apply_N_times(_ServerName, _F, 0) ->
  ok;

lb_apply_N_times(_, F, N) ->
  case N rem 5 of
    0 ->
      F(crud_db_server1, N);
    1 ->
      F(crud_db_server2, N);
    2 ->
      F(crud_db_server3, N);
    3 ->
      F(crud_db_server4, N);
    _ ->
      F(crud_db_server5, N)
  end,
  lb_apply_N_times(crud_db_server5, F, N - 1).


list_test_(Data) ->
  lager:info("===== Data: ~p ====", [Data]),
  [
    {"Load balancer CRUD test", lb_performance_crud_test_(Data)},
    {"Standalone CRUD test", standalone_performance_crud_test_(Data)},
    {"Load balancer save test", lb_performance_save_test_(Data)},
    {"Standalone save test", standalone_performance_save_test_(Data)}
  ].

start_balancers_test_() ->
  {inorder,
    [
      {setup, fun setup/0, fun cleanup/1, fun list_test_/1}
    ]
  }.

save_message(ServerName, N) ->
  Message = #message{correlation_id = N,
    type = message,
    payload = <<"SUCCESS">>,
    status = success},
  crud_db_server:call_save(ServerName, Message).

crud(ServerName, N) ->
  Message = #message{correlation_id = N,
    type = message,
    payload = <<"SUCCESS">>,
    status = success},
  crud_db_server:call_save(ServerName, Message),
  Message = crud_db_server:find(ServerName, message, N),
  crud_db_server:delete(ServerName, message, N).