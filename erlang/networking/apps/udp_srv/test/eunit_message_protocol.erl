%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2021 16:10
%%%-------------------------------------------------------------------
-module(eunit_message_protocol).
-author("alex").

-include_lib("eunit/include/eunit.hrl").
-include("message_protocol.hrl").

simple_test() ->
  ?assert(true).

setup() ->
  application:ensure_all_started(udp_srv),
  lager:info("applications started successfully").

cleanup(_D) ->
  app_supervisor:terminate_child(udp_srv),
  application:stop(lager).

eunit_suite1_(Data) ->
  lager:info("eunit_suite1_ is starting..."),
  [
    {"Create message with correlation id and payload", test_udp_message_with_correlation_id_and_payload(Data)},
    {"Format binaries and padding test", test_format_binaries(Data)},
    {"Performance test", generate_1M_messages_test(Data)}
  ].


message_protocol_test_() ->
  {inorder,
    [
      {setup, fun setup/0, fun cleanup/1, fun eunit_suite1_/1}
    ]
  }.
test_udp_message_with_correlation_id_and_payload(_D) ->
    Payload = <<"0123456789ABCDEF0123456789ABCDEF">>,
    CorrelationId = uuid:get_v4(),
    lager:info("Correlation_id: ~p, Payload: ~p", [CorrelationId, Payload]),
    UDPMessage = udp_message:udp_message(CorrelationId, Payload),
    ?assertEqual(size(UDPMessage), size(Payload) +  size(CorrelationId) + 5),
    ?_assert(true).

test_format_binaries(_D) ->
  Bin1 = <<70>>,
  Bin2 = <<Bin1/binary, 71>>,
  Bin3 = <<Bin2/binary, 72>>,
  Bin4 = <<Bin3/binary, 73>>,
  Bin5 = <<Bin4/binary, 74>>,
  Bin6 = <<Bin5/binary, 75>>,
  Bin7 = <<Bin6/binary, 76>>,
  Bin8 = <<Bin7/binary, 77>>,
  Bin9 = <<Bin8/binary, 78>>,

  P1 = udp_message:format_binary(Bin1, 8),
  P2 = udp_message:format_binary(Bin2, 8),
  P3 = udp_message:format_binary(Bin3, 8),
  P4 = udp_message:format_binary(Bin4, 8),
  P5 = udp_message:format_binary(Bin5, 8),
  P6 = udp_message:format_binary(Bin6, 8),
  P7 = udp_message:format_binary(Bin7, 8),
  P8 = udp_message:format_binary(Bin8, 8),
  Error = udp_message:format_binary(Bin9, 8),

  ?assertEqual(error, Error),

  ?assertEqual(size(P1), 8),
  ?assertEqual(size(P2), 8),
  ?assertEqual(size(P3), 8),
  ?assertEqual(size(P4), 8),
  ?assertEqual(size(P5), 8),
  ?assertEqual(size(P6), 8),
  ?assertEqual(size(P7), 8),
  ?assertEqual(size(P8), 8),

  ?assertEqual(P1, <<0, 0, 0, 0, 0, 0, 0, 70>>),
  ?assertEqual(P2, <<0, 0, 0, 0, 0, 0, 70, 71>>),
  ?assertEqual(P3, <<0, 0, 0, 0, 0, 70, 71, 72>>),
  ?assertEqual(P4, <<0, 0, 0, 0, 70, 71, 72, 73>>),
  ?assertEqual(P5, <<0, 0, 0, 70, 71, 72, 73, 74>>),
  ?assertEqual(P6, <<0, 0, 70, 71, 72, 73, 74, 75>>),
  ?assertEqual(P7, <<0, 70, 71, 72, 73, 74, 75, 76>>),
  ?assertEqual(P8, Bin8),

  ?_assert(true).


create_simple_message_test() ->

  Payload64 = <<"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF">>,
  Payload256 = <<Payload64/binary,Payload64/binary, Payload64/binary, Payload64/binary>>,
  Payload1K= <<Payload256/binary,Payload256/binary, Payload256/binary, Payload256/binary>>,
  Payload12K =
    <<Payload1K/binary,Payload1K/binary, Payload1K/binary, Payload1K/binary,
    Payload1K/binary,Payload1K/binary, Payload1K/binary, Payload1K/binary,
    Payload1K/binary,Payload1K/binary, Payload1K/binary, Payload1K/binary>>,

  CorrelationId = <<1,2, 3, 4, 5>>,
  Order = <<1>>,

  SimpleMessage64 = udp_message:create_simple_message(Payload64),
  ?assertEqual(64 + 8 + 8 +4, size(SimpleMessage64)),
  <<C:64, O:64, S:32, Payload64/binary>> = SimpleMessage64,

  ?assertEqual(0, C),
  ?assertEqual(0, 0),
  ?assertEqual(64, S),

  SimpleMessage12K = udp_message:create_simple_message(Payload12K),
  ?assertEqual(size(Payload12K) + 8 + 8 +4, size(SimpleMessage12K)),
  <<C:64, O:64, S12K:32, Payload12K/binary>> = SimpleMessage12K,

  ?assertEqual(0, C),
  ?assertEqual(0, 0),
  ?assertEqual(12288, S12K),

  ?_assert(true).

create_ack_message_test() ->
  ID = 121234,
  CorrelationID = udp_message:format_binary(binary:encode_unsigned(ID), 8),
  ?assertEqual(size(CorrelationID), 8),
  Payload64 = <<"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF">>,
  AcknowledgedMsg = udp_message:create_ack_message(CorrelationID, Payload64),
  ?assertEqual(64 + 8 + 8 +4, size(AcknowledgedMsg)),
  <<C:64, O:64, S:32, Payload64/binary>> = AcknowledgedMsg,
  ?assertEqual(ID, C),
  ?assertEqual(0, 0),
  ?assertEqual(64, S).

create_ordered_message_test() ->
  No = 121234,
  ID = 252534,
  Order = udp_message:format_binary(binary:encode_unsigned(No), 8),
  CorrelationId = udp_message:format_binary(binary:encode_unsigned(ID), 8),
  ?assertEqual(size(Order), 8),
  ?assertEqual(size(CorrelationId), 8),
  Payload64 = <<"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF">>,
  Message = udp_message:create_message(CorrelationId, Order, Payload64),
  ?assertEqual(64 + 8 + 8 +4, size(Message)),
  <<CID:64, OID:64, S:32, Payload64/binary>> = Message,
  ?assertEqual(ID, CID),
  ?assertEqual(No, OID),
  ?assertEqual(64, S).

process_valid_message_test() ->
  No = 121234,
  ID = 252534,
  Order = udp_message:format_binary(binary:encode_unsigned(No), 8),
  CorrelationId = udp_message:format_binary(binary:encode_unsigned(ID), 8),
  ?assertEqual(size(Order), 8),
  ?assertEqual(size(CorrelationId), 8),
  Payload64 = <<"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF">>,
  ValidMessage = udp_message:create_message(CorrelationId, Order, Payload64),
  Message = udp_message:process_message(ValidMessage),
  #message{payload = P, correlation_id = CID, order_number = OID} = Message,
  ?assertEqual(P, Payload64),
  ?assertEqual(CID, ID),
  ?assertEqual(OID, No),
  ?assertEqual(#message{payload = Payload64, correlation_id = ID, order_number = No, type = message}, Message).

acknowledged_message_test() ->
  No = 121234,
  ID = 252534,
  CorrelationId = udp_message:format_binary(binary:encode_unsigned(ID), 8),
  Message = <<6, CorrelationId/binary>>,
  #message{
    type = Type,
    correlation_id = CID,
    order_number = ON,
    payload = P
  } = udp_message:process_message(Message),
  ?assertEqual(Type, ack),
  ?assertEqual(P, undefined),
  ?assertEqual(CID, ID),
  ?assertEqual(ON, undefined).

process_invalid_message_test() ->
  No = 121234,
  ID = 252534,
  Order = udp_message:format_binary(binary:encode_unsigned(No), 8),
  CorrelationId = udp_message:format_binary(binary:encode_unsigned(ID), 8),
  Size = udp_message:format_binary(binary:encode_unsigned(63), 4),
  ?assertEqual(size(Order), 8),
  ?assertEqual(size(CorrelationId), 8),
  Payload64 = <<"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF">>,
  InvalidMessage = <<CorrelationId/binary, Order/binary, Size/binary, Payload64/binary>>,
  Msg = udp_message:process_message(InvalidMessage),
  ?assertEqual(error, Msg).

generate_1M_messages_test(_) ->
  Payload64 = <<"0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF">>,
  No = 121234,
  ID = 252534,
  Order = udp_message:format_binary(binary:encode_unsigned(No), 8),
  CorrelationId = udp_message:format_binary(binary:encode_unsigned(ID), 8),
  lists:foreach(
    fun(_) ->
      udp_message:create_message(CorrelationId, Order, Payload64)
    end,
    lists:seq(1, 1000000)),
  {Duration1, ok} = timer:tc(fun() ->  lists:foreach(
    fun(_) ->
      udp_message:create_message(CorrelationId, Order, Payload64)
    end,
    lists:seq(1, 1000000)) end),
  lager:info("Generating of 1M messages: ~p", [Duration1]),
  ValidMessage = udp_message:create_message(CorrelationId, Order, Payload64),
  {Duration2, ok} = timer:tc(fun() ->  lists:foreach(
    fun(_) ->
      udp_message:process_message(ValidMessage)
    end,
    lists:seq(1, 1000000)) end),
  lager:info("Processing of 1M messages: ~p", [Duration2]),
  ?_assert(true).