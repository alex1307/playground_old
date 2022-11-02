%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2021 15:27
%%%-------------------------------------------------------------------
-module(load_balancer).
-author("alex").

-behaviour(application).

-include_lib("utils/include/servers.hrl").

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  application:ensure_all_started(udp_srv),
  Servers = application:get_env(load_balancer, hosts, [{localhost, {0, 0, 0, 0}}]),
  lists:foreach(
    fun({ServerName, _IP}) ->
      true = is_process_alive(whereis(ServerName))
    end,
    Servers),
  Workers = case length(Servers) of
              1 ->
                [server1];

              2 ->
                app_supervisor:start_child(?WORKER1_SUP, load_balancer_srv, start_link, [?WORKER1_SRV, [server1], call]),
                app_supervisor:start_child(?WORKER2_SUP, load_balancer_srv, start_link, [?WORKER2_SRV, [server2], call]),
                [?WORKER1_SRV, ?WORKER2_SRV];

              3 ->
                app_supervisor:start_child(?WORKER1_SUP, load_balancer_srv, start_link, [?WORKER1_SRV, [server1, server2], call]),
                app_supervisor:start_child(?WORKER2_SUP, load_balancer_srv, start_link, [?WORKER2_SRV, [server2, server3], call]),
                app_supervisor:start_child(?WORKER3_SUP, load_balancer_srv, start_link, [?WORKER3_SRV, [server1, server3], call]),
                [?WORKER1_SRV, ?WORKER2_SRV, ?WORKER3_SRV];

              4 ->
                app_supervisor:start_child(?WORKER1_SUP, load_balancer_srv, start_link, [?WORKER1_SRV, [server1, server2], call]),
                app_supervisor:start_child(?WORKER2_SUP, load_balancer_srv, start_link, [?WORKER2_SRV, [server3, server4], call]),
                app_supervisor:start_child(?WORKER3_SUP, load_balancer_srv, start_link, [?WORKER3_SRV, [server1, server3], call]),
                app_supervisor:start_child(?WORKER4_SUP, load_balancer_srv, start_link, [?WORKER4_SRV, [server2, server4], call]),
                app_supervisor:start_child(?WORKER5_SUP, load_balancer_srv, start_link, [?WORKER5_SRV, [server1, server4], call]),
                app_supervisor:start_child(?WORKER6_SUP, load_balancer_srv, start_link, [?WORKER6_SRV, [server2, server3], call]),
                [?WORKER1_SRV, ?WORKER2_SRV, ?WORKER3_SRV, ?WORKER4_SRV, ?WORKER5_SRV, ?WORKER6_SRV];

              5 ->
                app_supervisor:start_child(?WORKER1_SUP, load_balancer_srv, start_link, [?WORKER1_SRV, [server1, server3, server5], call]),
                app_supervisor:start_child(?WORKER2_SUP, load_balancer_srv, start_link, [?WORKER2_SRV, [server2, server4], call]),
                app_supervisor:start_child(?WORKER3_SUP, load_balancer_srv, start_link, [?WORKER3_SRV, [server1, server2, server4], call]),
                app_supervisor:start_child(?WORKER4_SUP, load_balancer_srv, start_link, [?WORKER4_SRV, [server3, server5], call]),
                app_supervisor:start_child(?WORKER5_SUP, load_balancer_srv, start_link, [?WORKER5_SRV, [server2, server3, server4], call]),
                app_supervisor:start_child(?WORKER6_SUP, load_balancer_srv, start_link, [?WORKER6_SRV, [server1, server5], call]),
                [?WORKER1_SRV, ?WORKER2_SRV, ?WORKER3_SRV, ?WORKER4_SRV, ?WORKER5_SRV, ?WORKER6_SRV];
              _ ->
                app_supervisor:start_child(?WORKER1_SUP, load_balancer_srv, start_link, [?WORKER1_SRV, [server1, server3, server5], call]),
                app_supervisor:start_child(?WORKER2_SUP, load_balancer_srv, start_link, [?WORKER2_SRV, [server2, server4, server6], call]),
                app_supervisor:start_child(?WORKER3_SUP, load_balancer_srv, start_link, [?WORKER3_SRV, [server1, server2, server4], call]),
                app_supervisor:start_child(?WORKER4_SUP, load_balancer_srv, start_link, [?WORKER4_SRV, [server3, server5, server6], call]),
                app_supervisor:start_child(?WORKER5_SUP, load_balancer_srv, start_link, [?WORKER5_SRV, [server2, server3, server4], call]),
                app_supervisor:start_child(?WORKER6_SUP, load_balancer_srv, start_link, [?WORKER6_SRV, [server1, server5, server6], call]),
                [?WORKER1_SRV, ?WORKER2_SRV, ?WORKER3_SRV, ?WORKER4_SRV, ?WORKER5_SRV, ?WORKER6_SRV]
            end,
  app_supervisor:start_child(?ROOT_LB_SUP, load_balancer_srv, start_link, [?ROOT_LB, Workers, call]),
  lager:info("Load balancer started successfully with workers [~p] and severs [~p]", [length(Workers), length(Servers)]),
  {ok, self()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
