%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2021 15:56
%%%-------------------------------------------------------------------
-author("alex").
-define(SOT, <<2>>).
-define(EOT, <<3>>).
-define(ENQ, <<5>>).
-define(ACK, <<6>>).
-define(NAK, <<21>>).
-define(SYN, <<22>>).
-define(RS, <<30>>).

-define(MESSAGE_SYNC, sync).
-define(MESSAGE, message).
-define(MESSAGE_ACK, ack).

-define(CORRELATION_ID, <<0, 0, 0, 0, 0, 0, 0, 0>>).
-define(SIZE, <<0, 0, 0, 0>>).
-define(ORDER, <<0, 0, 0, 0, 0, 0, 0, 0>>).

-define(MESSAGE_SRV, message_srv).
-define(ACK_SRV, ack_srv).
-define(SYNC_SRV, sync_srv).

-record(message, {
  type :: atom(),
  payload :: binary(),
  correlation_id :: binary(),
  order_number :: integer()
}).


