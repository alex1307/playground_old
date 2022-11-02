%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 11-Dec-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(udp_srv).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-behaviour(application).
-include("message_protocol.hrl").
-include_lib("utils/include/servers.hrl").
%% Application callbacks
-export([
  start/2,
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
  lager:info("Starting main app"),
  Port = application:get_env(udp_srv, port, 5663),
  IPs = application:get_env(udp_srv, hosts, [{localhost, {0, 0, 0, 0}}]),
  lists:foreach(
    fun({ServerName, IP}) ->
      Bin = atom_to_binary(ServerName),
      SupName = <<Bin/binary, "_sup">>,
      app_supervisor:start_child(binary_to_atom(SupName), udp_server, start_link, [ServerName, Port, IP])
    end,
    IPs),
  app_supervisor:start_child(messages_sup, message_processor, start_link, [?MESSAGE_SRV]),
  app_supervisor:start_child(ack_sup, message_processor, start_link, [?ACK_SRV]),
  app_supervisor:start_child(sync_sup, message_processor, start_link, [?SYNC_SRV]),
  app_supervisor:start_child(outgoing_msg_counter_sup , message_counter, start_link, [?OUTGOING_MESSAGE_COUNTER_SRV]),
  app_supervisor:start_child(incoming_msg_counter_sup, message_counter, start_link, [?INCOMING_MESSAGE_COUNTER_SRV]),
  message_counter:set_interval(?INCOMING_MESSAGE_COUNTER_SRV, 1000),
  message_counter:set_interval(?OUTGOING_MESSAGE_COUNTER_SRV, 1000),
  print_cheat_sheet(),
  {ok, self()}.

print_cheat_sheet() ->
  lager:info("====================================== Useful commands ======================================"),
  lager:info("OUT_SRV = ~p", [?OUTGOING_MESSAGE_COUNTER_SRV]),
  lager:info("IN_SRV = ~p", [?INCOMING_MESSAGE_COUNTER_SRV]),
  lager:info("F = fun(Server, Message) -> gen_server:call(Server, {message, Message}) end."),
  lager:info("M = scenarios:create_message_with_size(2047)."),
  lager:info("W = fun(Server, Message, Iterations) -> lists:foreach(fun(_) ->  F(Server, Message) end, lists:seq(1, Iterations)) end."),
  lager:info("S = fun(Server, Message, Iterations) -> spawn(fun() -> W(Server, Message, Iterations) end) end."),
  lager:info("S(worker1, M, 100000), S(worker2, M, 100000), S(worker3, M, 100000)"),
  lager:info("=============================================================================================").

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
