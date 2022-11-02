
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 11-Dec-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(services).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-behaviour(application).

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
%%  app_supervisor:start_child(request_processor_sup, request_processor, start_link, []),
%%  app_supervisor:start_child(ack_message_listener_sup, ack_message_listener, start_link, []),
%%  callback_subscriber:subscribe(ack_message_listener),
%%  [ack_message_listener] = callback_subscriber:subscribers(),
  lager:info("*** Application [services] is started successfully. ***"),
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

