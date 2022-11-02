%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jan 2021 8:14
%%%-------------------------------------------------------------------
-module(request_processor_performance_eunit).
-author("alex").

-include_lib("eunit/include/eunit.hrl").
-include_lib("utils/include/mnesia_model.hrl").

ensure_started_test(_) ->
  ?_assert(is_process_alive(whereis(crud_db_server))),
  ?_assert(is_process_alive(whereis(jiffy_srv))),
  ?_assert(is_process_alive(whereis(elavro_srv))),
  ?_assert(is_process_alive(whereis(request_processor))).




standalone_performance_crud_test_(_) ->
%%  lager:info("Memory: ~p", [erlang:memory()]),
  lager:info("Standalone CRUD: ~p", [timer:tc(fun apply_N_times/2, [fun crud/1,100000])]),
%%  lager:info("Memory: ~p", [erlang:memory()]),
  ?_assert(true).

standalone_performance_save_test_(_) ->
%%  lager:info("Memory: ~p", [erlang:memory()]),
  lager:info("Started: ~p", [erlang:system_time(millisecond)]),
  lager:info("Only save: ~p", [timer:tc(fun apply_N_times/2, [fun save_message/1,100000])]),
  lager:info("Finished: ~p", [erlang:system_time(millisecond)]),
  lager:info("Standalone process: ~p", [erlang:process_info(whereis(crud_db_server))]),
%%  lager:info("Memory: ~p", [erlang:memory()]),
  ?_assert(true).



apply_N_times( _F, 0) ->
  ok;

apply_N_times(F, N) ->
  F(N),
  apply_N_times(F, N - 1).



list_test_(Data) ->
  lager:info("===== Data: ~p ====", [Data]),
  [
    {"Standalone CRUD test", ensure_started_test(Data)},
    {"Standalone CRUD test", standalone_performance_crud_test_(Data)},
    {"Standalone save test", standalone_performance_save_test_(Data)}
  ].

start_balancers_test_() ->
  {inorder,
    [
      {setup, fun services_eunit_setup:setup/0, fun services_eunit_setup:cleanup/1, fun list_test_/1}
    ]
  }.

save_message(N) ->
  Message = message(N),
  request_processor:save_message(Message).

crud(N) ->
  Json = message(N),
  Message = request_processor:save_message(Json),
  Message = request_processor:find_message(Message#message.correlation_id),
  request_processor:delete_message(Message#message.correlation_id).


message(N) ->
  Start = <<"{\"id\":\"">>,
  ID = integer_to_binary(N),
  Rest = <<"\",\"from\":\"James\",\"to\":\"Bond\",\"subject\":\"007\",\"body\":\"Kill them all\",\"attachment\":\"none\"}">>,
  <<Start/binary, ID/binary, Rest/binary>>.