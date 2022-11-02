%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jun 2020 17:54
%%%-------------------------------------------------------------------
-module(http_server_app).
-author("alex").

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).


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
  HTTP_Port = application:get_env(http_server_app, http_port, 3444),
  HTTPS_Port = application:get_env(http_server_app, https_port, 3443),
  CertDir = "priv",
  CertFile = filename:join(CertDir, "ayagasha.crt"),
  KeyFile = filename:join(CertDir, "ayagasha.key"),

  lager:info("CertFile ~p ~n KeyFile ~p", [CertFile, KeyFile]),
  HttpOptions = [{callback, http_handler}, {port, HTTP_Port}, {host, <<"0.0.0.0">>}],
  SSLOptions = [ {callback, http_handler},
    {host, <<"0.0.0.0">>},
    {port, HTTPS_Port},
    ssl,
    {keyfile, KeyFile},
    {certfile, CertFile}],

  app_supervisor:start_child(http_elli_handler, elli, start_link, [HttpOptions]),
  app_supervisor:start_child(https_elli_handler, elli, start_link, [SSLOptions]),
  app_supervisor:start_child(authorization_srv, authorization_srv, start_link, []),
  lager:info("#### Children ~p",[supervisor:which_children(app_supervisor)]),
  lager:info("HTTP(~p) and HTTPS(~p) servers have been started successfully.", [HTTP_Port, HTTPS_Port]),
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