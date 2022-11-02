%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2021 16:37
%%%-------------------------------------------------------------------
-module(scenarios).
-author("alex").

%% API
-export([
  configure/2,
  send_messages/3,
  send_messages/5,
  mac_setup/0,
  pi_setup/0,
  create_message_with_size/1,
  send_messages/2,
  send_message/2,
  send_bulk_of_messages/2]).

configure(IP, Port) ->
  IPs = application:get_env(udp_srv, hosts, [{localhost, {0, 0, 0, 0}}]),
  lists:foreach(
    fun({ServerName, _IP}) ->
      udp_server:configure(ServerName, IP, Port),
      lager:info("Server ~p has been configured with destination {~p, ~p}", [ServerName, IP, Port])
    end,
    IPs).

send_messages(ServerName, Count, Size) ->
  send_messages(ServerName, Count, Size, 20, 1).

send_messages(ServerName, Count, Size, RemNumber, SleepMs) ->
  Bin = create_message_with_size(Size),
  {Duration, ok} = timer:tc(
    fun() ->
      lists:foreach(
        fun(I) ->
          case I rem RemNumber of
            0 -> timer:sleep(SleepMs),
              udp_server:send_message(ServerName, Bin);
            _ ->
              udp_server:send_message(ServerName, Bin)
          end
        end,
        lists:seq(1, Count))
    end
  ),
  lager:info("Sending ~p mesages with size ~p takes ~p millis", [Count, size(Bin), trunc(Duration / 1000)]).


mac_setup() ->
  app_supervisor:start_child(sup_worker1, load_balancer, start_link, [worker1, [server1, server2], call]),
  app_supervisor:start_child(sup_worker2, load_balancer, start_link, [worker2, [server2, server3], call]),
  app_supervisor:start_child(sup_worker3, load_balancer, start_link, [worker3, [server1, server3], call]),
  app_supervisor:start_child(sup_lb_root, load_balancer, start_link, [root, [worker1, worker2, worker3], call]),
  udp_server:configure(server1, {10, 51, 9, 101}, 5663),
  udp_server:configure(server1, {10, 51, 9, 11}, 5663),
  udp_server:configure(server2, {10, 51, 9, 11}, 5663),
  udp_server:configure(server2, {10, 51, 9, 12}, 5663),
  udp_server:configure(server3, {10, 51, 9, 12}, 5663),
  udp_server:configure(server3, {10, 51, 9, 101}, 5663),
  load_balancer:configure_delay_and_limiter(root, 1, 20),
  load_balancer:configure_delay_and_limiter(worker1, 1, 25),
  load_balancer:configure_delay_and_limiter(worker2, 1, 25),
  load_balancer:configure_delay_and_limiter(worker3, 1, 25).

pi_setup() ->
  app_supervisor:start_child(sup_worker1, load_balancer, start_link, [worker1, [server1, server3, server5], call]),
  app_supervisor:start_child(sup_worker2, load_balancer, start_link, [worker2, [server2, server4, server6], call]),
  app_supervisor:start_child(sup_worker3, load_balancer, start_link, [worker3, [server1, server3, server6], call]),
  app_supervisor:start_child(sup_worker4, load_balancer, start_link, [worker4, [server2, server4, server5], call]),
  app_supervisor:start_child(sup_lb_root, load_balancer, start_link, [root, [worker1, worker2, worker3, worker4], call]),
  udp_server:configure(server1, {10, 51, 9, 241}, 5663),
  udp_server:configure(server2, {10, 51, 9, 21}, 5663),
  udp_server:configure(server3, {10, 51, 9, 31}, 5663),
  udp_server:configure(server4, {10, 51, 9, 21}, 5663),
  udp_server:configure(server5, {10, 51, 9, 241}, 5663),
  udp_server:configure(server6, {10, 51, 9, 31}, 5663),
  load_balancer:configure_delay_and_limiter(root, 1, 20),
  load_balancer:configure_delay_and_limiter(worker1, 1, 25),
  load_balancer:configure_delay_and_limiter(worker2, 1, 25),
  load_balancer:configure_delay_and_limiter(worker3, 1, 25),
  load_balancer:configure_delay_and_limiter(worker4, 1, 25).


create_message_with_size(Size) ->
  P = <<"0123456789ABCDEF">>,
  N = trunc(Size / 16) + 1,
  L = lists:map(fun(_) -> P end, lists:seq(1, N)),
  list_to_binary(L).



send_message(worker1, Message) ->
  spawn(fun() -> gen_server:call(worker1, {message, Message}) end),
  worker2;

send_message(worker2, Message) ->
  spawn(fun() -> gen_server:call(worker2, {message, Message}) end),
  worker3;

send_message(worker3, Message) ->
  spawn(fun() -> gen_server:call(worker3, {message, Message}) end),
  worker4;

send_message(worker4, Message) ->
  spawn(fun() -> gen_server:call(worker4, {message, Message}) end),
  worker1;

send_message(_Message, _Any) ->
  error.


send_messages(Server, [H|T]) when is_atom(Server) andalso is_binary(H) ->
  Next = send_message(Server, H),
  send_messages(Next, T);

send_messages(Server, []) when is_atom(Server) ->
  ok;

send_messages(_Current, _M) ->
  error.

send_bulk_of_messages(Worker, Messages) ->
  spawn(fun() -> send_messages(Worker, Messages) end).



