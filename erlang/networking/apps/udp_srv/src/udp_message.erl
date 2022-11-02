%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2021 15:57
%%%-------------------------------------------------------------------
-module(udp_message).
-author("alex").
-include("message_protocol.hrl").
%% API
-export([
  ack_message/1,
  create_ack_message/2,
  create_ordered_message/2,
  create_simple_message/1,
  create_message/3,
  format_binary/2,
  process_ack/1,
  process_message/1,
  process_udp_message/1
]).

process_udp_message(<<>>) ->
  {<<>>, <<>>};

process_udp_message(Bin) when is_binary(Bin) ->
  L = binary:split(Bin, [?SOT, ?RS, ?EOT], [global, trim_all]),
  case L of
    [CorrelationID, Size, Payload] when
      is_binary(Payload) andalso is_binary(CorrelationID) andalso is_binary(Size) ->
      [CorrelationID, Size, Payload];

    [Payload] when is_binary(Payload) ->
      [Payload];
    _Err -> error
  end.

ack_message(<<>>) ->
  lager:error("correlation_id.missing"),
  error;

ack_message(CorrelationID) when is_binary(CorrelationID) ->
  <<?SOT, ?ACK, CorrelationID/binary, ?EOT>>;
ack_message(_) ->
  lager:error("binary.required"),
  error.

process_ack(<<2, 6, _A>> = Bin) ->
  {ack, binary:split(Bin, [?SOT, ?ACK, ?EOT], [global, trim_all])};

process_ack(<<2, 21, _>> = Bin) ->
  {nak, binary:split(Bin, [?SOT, ?ACK, ?EOT], [global, trim_all])};

process_ack(_) ->
  lager:error("invalid.ack.message"),
  error.

format_binary(Source, Len) when is_binary(Source) and is_integer(Len) ->
  case Len - size(Source) of
    0 ->
      Source;
    N when N > 0 ->
      Padding = (N * 8),
      <<0:Padding, Source/binary>>;
    _Err ->
      error
  end.

create_simple_message(Payload) ->
  create_message(<<>>, <<>>, Payload).

create_ack_message(CorrelationID, Payload) ->
  create_message(CorrelationID, <<>>, Payload).

create_ordered_message(OrderNumber, Payload) ->
  create_message(<<>>, OrderNumber, Payload).

create_message(_, _, <<>>) ->
  lager:error("~p: ~p empty message", [?MODULE, ?LINE]),
  error;

create_message(<<>>, <<>>, Payload) when is_binary(Payload) ->
  create_message(?CORRELATION_ID, ?ORDER, Payload);

create_message(<<>>, Order, Payload) when is_binary(Payload) and is_binary(Order) ->
  create_message(?CORRELATION_ID, Order, Payload);

create_message(CorrelationID, <<>>, Payload) when is_binary(Payload) and is_binary(CorrelationID) ->
  create_message(CorrelationID, ?ORDER, Payload);


create_message(CorrelationID, Order, Payload) when is_binary(Payload) andalso is_binary(Order) andalso is_binary(CorrelationID) ->
  Size = format_binary(binary:encode_unsigned(size(Payload)), 4),
  case Size of
    error ->
      lager:error("~p: ~p incorrect.size. Max size: 2147483648 bytes", [?MODULE, ?LINE]),
      error;
    _OK ->
      <<CorrelationID/binary, Order/binary, Size/binary, Payload/binary>>
  end;

create_message(_CorrelationID, _Order, _Payload) ->
  lager:error("~p: ~p invalid parameters", [?MODULE, ?LINE]),
  error.

process_message(<<6, CorrelationId:64>>) ->
  #message{
    type = ack,
    correlation_id = CorrelationId
  };

process_message(<<22, CorrelationId:64>>) ->
  #message{
    type = sync,
    correlation_id = CorrelationId
  };

process_message(<<CorrelationId:64, Order:64, Size:32, Payload/binary>>) ->
  case size(Payload) of
    Size ->
      #message{
        type = message,
        payload = Payload,
        correlation_id = CorrelationId,
        order_number = Order
      };
    Err ->
      lager:error("~p: ~p Invalid message. Size: ~p. Expected: ~p", [?MODULE, ?LINE, Err, Size]),
      error
  end;

process_message(_Ant) ->
  lager:error("~p: ~p Invalid binary message", [?MODULE, ?LINE]),
  error.