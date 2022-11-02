%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2016 2:12 PM
%%%-------------------------------------------------------------------
-author("alex").
-define(TYPES_HEADER, types).

-ifndef(CONSTANTS_HEADER).
-include("constants.hrl").
-endif.

-type ftdi_config() :: serial | gpio.

-type user_action() :: ?UNDEFINED | 'START' | 'REQUEST' | 'CONFIRM' | 'CANCEL' |
'BACK' | 'RETRY' | 'PRINT_RECEIPT' | 'EMAIL_RECEIPT'.

-type primary_source()  :: 'CAMERA' | 'TERMINAL' | null.
-type qr_code_type()    :: 'USERQR' | 'QRCODE' | 'RESERVATION'.
-type message_type()    :: ?OK | ?ERROR | ?TIMEOUT | 'alert'.
-type device()          :: ?UNDEFINED | barrier | ?CAMERA | ?PRINTER | 'payment_terminal' | ?SCANNER | ?BUTTON | ?RFID | fiscal_printer.
-type config()          :: ?SCANNER | ?CAMERA | both.
-type status()          :: ?UNDEFINED | ?OK | ?ERROR | ?TIMEOUT.
-type transaction_state() :: ?UNDEFINED.
-type registration_status() :: idle | active | processing | invalid | not_paid | success.