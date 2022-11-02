
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 11-Dec-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(handlers).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1,
  normalize/1]).

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
start(_StartType, _Args) ->
  {ok, AvroDir} = application:get_env(handlers, avro_schema_dir),
  app_supervisor:start_child(elavro_srv, elavro_srv, start_link, [AvroDir]),
  app_supervisor:start_child(jiffy_srv, jiffy_srv, start_link, []),
  lager:info("*** Application [handlers] is started successfully. ***"),
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
normalize([{V}]) ->
  [V];
normalize([{H} | T]) ->
  lists:append([H], normalize(T));
normalize({V}) -> V;
normalize(V) -> V.