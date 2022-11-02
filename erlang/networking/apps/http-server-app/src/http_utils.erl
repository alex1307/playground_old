%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Feb 2021 11:19
%%%-------------------------------------------------------------------
-module(http_utils).
-author("alex").

%% API
-export([
  http_response/2,
  jwt_create/2,
  verify_bearer/1,
  verify_token/1
]).

-define(HTTP_BAD_REQUEST, 400).
-define(HTTP_NOT_AUTHORIZED, 401).
-define(HTTP_FORBIDDEN, 403).
-define(HTTP_NOT_FOUND, 404).
-define(HTTP_NOT_ACCEPTABLE, 406).
-define(HTTP_TIMEOUT, 408).


-define(HTTP_OK, 200).
-define(HTTP_CREATED, 201).
-define(HTTP_ACCEPTED, 204).
-define(HTTP_UNAUTHORIZED, 401).

-define(HTTP_SERVER_ERROR, 500).
-define(HTTP_NOT_IMPLEMENTED, 501).
-define(HTTP_TEMP_UNAVAILABLE, 503).

-define(JSON_TXT_MSG, <<"{ \"value\": \"_replace_it_\"}">>).
-define(JSON_INT_MSG, <<"{ \"value\": _replace_it_}">>).
-define(JSON_BOOLEAN_MSG, <<"{ \"value\": _replace_it_}">>).
-define(JSON_NULL_MSG, <<"{ \"value\": null}">>).
-define(JSON_EMPTY_MSG, <<"{ \"value\": \"\"}">>).

-define(JWT_KEY, <<"uPy1.euE4TePGq5GafL0Jg~I67RsdGYNvb1Q_lemoMCWmVfAqapDoa26P14ZqgcYLiFz5ZZAQnSMHdVO.RbO6LNCpOLjmkn3Lf-vS7xY_-R1GTSGj9_YXTkZmZd8yvfZ">>).

-define(BEARER, <<"Bearer ">>).

-define(HEADER, [
  {<<"Accept">>, <<"application/json">>},
  {<<"Access-Control-Allow-Origin">>, <<"*">>},
  {<<"Access-Control-Allow-Methods">>, <<"OPTIONS,GET,PUT,POST,DELETE,HEAD">>},
  {<<"Access-Control-Allow-Headers">>, <<"X-Requested-With,Content-Type,Accept,Origin,Token">>}
]).

-define(POST_HEADER(UUID), [
  {<<"correlation-id">>, UUID},
  {<<"Accept">>, <<"application/json">>},
  {<<"Content-Type">>, <<"application/json">>},
  {<<"Access-Control-Allow-Origin">>, <<"*">>},
  {<<"Access-Control-Allow-Methods">>, <<"OPTIONS,GET,PUT,POST,DELETE,HEAD">>},
  {<<"Access-Control-Allow-Headers">>, <<"X-Requested-With,Content-Type,Accept,Origin,Token">>}
]).

-include_lib("utils/include/service_space.hrl").

http_response(BadRequest, Message)
  when
  BadRequest == ?INVALID_JSON orelse
    BadRequest == ?INVALID_AVRO orelse
    BadRequest == ?INVALID ->

  {?HTTP_BAD_REQUEST, ?HEADER, binary_to_list(Message)};

http_response(?EMPTY, []) ->
  {?HTTP_BAD_REQUEST, ?HEADER, "{\"error\":\"blank.or.empty.body\"}"};

http_response(Status, Message)
  when
  Status == ?SERVER_IS_NOT_RUNNING ->
  {?HTTP_TEMP_UNAVAILABLE, ?HEADER, Message};

http_response(Status, Message)
  when
  Status == ?RECORD_NOT_FOUND orelse
    Status == ?NOT_FOUND ->
  {?HTTP_NOT_FOUND, ?HEADER, Message};

http_response(ok, Payload) ->
  {?HTTP_OK, ?HEADER, Payload};

http_response(accepted, CorrelationId) ->
  {?HTTP_ACCEPTED, ?POST_HEADER(CorrelationId), <<>>};

http_response(created, CorrelationId) ->
  {?HTTP_CREATED, ?POST_HEADER(CorrelationId), <<>>};

http_response(unauthorized, _CID) ->
  {?HTTP_UNAUTHORIZED, ?POST_HEADER(<<"">>), <<"{ \"message\": \"unauthorized.access\"}">>};

http_response(_Status, Message) ->
  {?HTTP_SERVER_ERROR, ?HEADER, Message}.

jwt_create(Username, Authorities) ->
  Now = erlang:system_time(second),
  Data = maps:from_list([
    {username, Username},
    {authorities, Authorities},
    {iat, Now},
    {exp, Now + 900}]),
  Token = jwerl:sign(Data, hs256, ?JWT_KEY),
%%  lager:info("Encoded token: ~p", [Token]),
  Token.

verify_token(Token) ->
  case jwerl:verify(Token, hs256, ?JWT_KEY) of
    Valid = {ok, Data} ->
%%      lager:info("Decoded token data: ~p", [Data]),
      Valid;
    {error, Reason} ->
      lager:error("Invalid token: ~p", [Reason]),
      ?INVALID
  end.

verify_bearer(<<"Bearer ", Token/binary>>) ->
  verify_token(Token);

verify_bearer(Token) ->
  lager:error("Invalid Bearer token: ~p", [Token]),
  ?INVALID.


