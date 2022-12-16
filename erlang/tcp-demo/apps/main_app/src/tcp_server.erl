%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2022, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 16-Dec-2022
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket}).

start_link(Socket) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Socket, []).

init(Socket) ->
  %% Start accepting requests
  %% We must cast this to the worker's process, as it blocks it.
  gen_server:cast(?MODULE, accept),
  {ok, #state{socket=Socket}}.

handle_cast(accept, State = #state{socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  send(AcceptSocket, "Hello", []),
  {noreply, State#state{socket=AcceptSocket}};
handle_cast(_, State) ->
  {noreply, State}.

handle_call(Bin, _From, State=#state{socket=Socket}) -> 
  send(Socket, Bin, []),
  {reply, ok, State};

handle_call(_E, _From, State) -> {noreply, State}.

handle_info({tcp, Socket, "quit"}, State) ->
  lager:info("Client disconnected"),
  gen_tcp:close(Socket),
  {stop, normal, State};
handle_info({tcp, Socket, Msg}, State) ->
  send(Socket, Msg, []),
  {noreply, State};
handle_info({tcp_closed, _Socket}, State) -> {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State};
handle_info(E, State) ->
  io:fwrite("unexpected: ~p~n", [E]),
  {noreply, State}.


terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

%% Send a message back to the client
send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, true}]),
  ok.