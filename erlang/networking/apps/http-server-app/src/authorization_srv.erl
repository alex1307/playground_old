%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(authorization_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([
  validate_bearer_token/1,
  check_bearer_token/1,
  login/2]).

-define(SERVER, ?MODULE).

-record(authorization_srv_state, {
    tokens::map(),
    last_login::map(),
    users::map(),
    authorities::map()
}).

-record(token, {
    iat::integer(),
    exp::integer(),
    username::binary(),
    authorities::[A::binary()]
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #authorization_srv_state{
      tokens = maps:new(),
      last_login = maps:new(),
      users = maps:new(),
      authorities = maps:new()}}.

handle_call({register, User, Pwd, Authorities}, _From, State = #authorization_srv_state{tokens = Tokens, users = Users, authorities = Auth, last_login = LoginHist}) ->
  Token = http_utils:jwt_create(User, Authorities),
  Now = erlang:system_time(second),
  {reply, Token, State#authorization_srv_state{
    tokens = maps:put(Token, User, Tokens),
    last_login = maps:put(User, Now, LoginHist),
    users = maps:put(User, Token, Users),
    authorities = maps:put(User, Authorities, Auth)}};


handle_call({login, User, Authorities}, _From, State = #authorization_srv_state{tokens = Tokens, users = Users, authorities = Auth, last_login = LoginHist}) ->
    Token = http_utils:jwt_create(User, Authorities),
    Now = erlang:system_time(second),
    {reply, Token, State#authorization_srv_state{
        tokens = maps:put(Token, User, Tokens),
        last_login = maps:put(User, Now, LoginHist),
        users = maps:put(User, Token, Users),
        authorities = maps:put(User, Authorities, Auth)}};

handle_call({bearer, <<"Bearer ", Token/binary>>}, _From, State = #authorization_srv_state{tokens = Tokens, last_login = LoginHist}) ->
     case maps:find(Token, Tokens) of
        {ok, User} ->
            Now = erlang:system_time(second),
            {reply, valid, State#authorization_srv_state{last_login = maps:put(User, Now, LoginHist)}};
        _Error ->
            lager:error("Invalid token: ~p", [Token]),
            {reply, invalid, State}
    end;

handle_call({validate, Bearer  = <<"Bearer ", Token/binary>>}, _From, State = #authorization_srv_state{tokens = Tokens, last_login = LoginHist}) ->
    case http_utils:verify_bearer(Bearer) of
        {ok, #token{username = Username}} ->
            Now = erlang:system_time(second),
            {reply, valid, State#authorization_srv_state{tokens = maps:put(Token, Username,Tokens),  last_login = maps:put(Username, Now, LoginHist)}};
        _Invalid ->
            {reply, invalid, State#authorization_srv_state{tokens = maps:remove(Token, Tokens)}}
    end;

handle_call({user, Bearer  = <<"Bearer ", Token/binary>>}, _From, State = #authorization_srv_state{tokens = Tokens}) ->
  case maps:find(Token, Tokens) of
    {ok, User} ->
      {reply, User, State};
    _Error ->
      lager:error("Invalid token: ~p", [Token]),
      {reply, not_found, State}
  end;

handle_call({last_login, User}, _From, State = #authorization_srv_state{last_login = LoginHist}) ->
  case maps:find(User, LoginHist) of
    {ok, Timestamp} ->
      {reply, Timestamp, State};
    _Error ->
      lager:error("User not found: ~p", [User]),
      {reply, not_found, State}
  end;

handle_call({authorities, User}, _From, State = #authorization_srv_state{authorities =  Auth}) ->
  case maps:find(User, Auth) of
    {ok, L} when is_list(L)->
      {reply, L, State};
    _Error ->
      lager:error("User not found: ~p", [User]),
      {reply, not_found, State}
  end;


handle_call(_Request, _From, State = #authorization_srv_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #authorization_srv_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #authorization_srv_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #authorization_srv_state{}) ->
  ok.

code_change(_OldVsn, State = #authorization_srv_state{}, _Extra) ->
  {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
login(ServerName, Username, Pwd) when is_binary(Username) andalso is_binary(Pwd) ->
  gen_server:call(ServerName, {login, Username, [<<"FULL_ACCESS">>]});

login(_ServerName, _Username, _Pwd) ->
  invalid_credentials.

login(Username, Pwd) ->
  login(?SERVER, Username, Pwd).

validate_bearer_token(ServerName, BearerToken) when is_binary(BearerToken)->
  gen_server:call(ServerName, {validate, BearerToken});

validate_bearer_token(_ServerName, _BearerToken) ->
  invalid_token.

validate_bearer_token(BearerToken) ->
  validate_bearer_token(?SERVER, BearerToken).

check_bearer_token(ServerName, BearerToken) when is_binary(BearerToken)->
  gen_server:call(ServerName, {bearer, BearerToken});

check_bearer_token(_ServerName, _BearerToken) ->
  invalid_token.

check_bearer_token(BearerToken) ->
  check_bearer_token(?SERVER, BearerToken).