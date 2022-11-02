%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2015 12:58 PM
%%%-------------------------------------------------------------------
-module(http_handler).
-author("alex").

%% API
-export([handle/2, handle_event/3]).
-include_lib("elli/include/elli.hrl").
-include_lib("utils/include/mnesia_model.hrl").
-include_lib("utils/include/servers.hrl").
-behaviour(elli_handler).
-record(state, {pid}).

-define(CROSS_DOMAIN_HEADER, [
  {<<"Accept">>, <<"application/json">>},
  {<<"Access-Control-Allow-Origin">>, <<"*">>},
  {<<"Access-Control-Allow-Methods">>, <<"OPTIONS,GET,PUT,POST,DELETE,HEAD">>},
  {<<"Access-Control-Allow-Headers">>, <<"X-Requested-With,Content-Type,Accept,Origin,Token">>}
]).

-define(CROSS_DOMAIN_HEADER(UUID), [
  {<<"correlation-id">>, UUID},
  {<<"Accept">>, <<"application/json">>},
  {<<"Content-Type">>, <<"application/json">>},
  {<<"Access-Control-Allow-Origin">>, <<"*">>},
  {<<"Access-Control-Allow-Methods">>, <<"OPTIONS,GET,PUT,POST,DELETE,HEAD">>},
  {<<"Access-Control-Allow-Headers">>, <<"X-Requested-With,Content-Type,Accept,Origin,Token">>}
]).

handle(Req, _Args) ->
  handle(Req#req.method, elli_request:path(Req), Req).

handle('POST', [<<"login">>], _Request) ->
  Token = authorization_srv:login(<<"U100_100">>, <<"">>),
  Json = [{<<"access_token">>, Token}],
  http_utils:http_response(ok, jiffy_srv:tuple_list_to_binary(Json));

handle('POST', [<<"message">>], Request) ->
  RList = elli_request:to_proplist(Request),
  Body = proplists:get_value(body, RList),
  Header = proplists:get_value(headers, RList),
  Bearer = proplists:get_value(<<"Authorization">>, Header),
  case authorization_srv:check_bearer_token(Bearer) of
    valid ->
      Binary = jiffy_srv:json_to_avro(message, Body),
      message_collector_srv:send_message(?MESSAGE_COLLECTOR_SRV,Binary),
      http_utils:http_response(created, common_utils:get_uuid());
    _Invalid -> http_utils:http_response(unauthorized, <<>>)
  end;


handle('GET', [<<"message">>, UUID], _Request) ->
  {Status, Payload} = request_processor:get_original_message(UUID),
  http_utils:http_response(Status, Payload);

handle(_, _, _Req) ->
  http_utils:http_response(?INVALID, <<"{\"message\": \"not implemented\"}">>).

handle_event(_Event, _Data, _Args) ->
  ok.


