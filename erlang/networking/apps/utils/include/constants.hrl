
-define(CONSTANTS_HEADER, constants).

-define(CAMERA,   camera).
-define(SCANNER,  scanner).
-define(BARRIER,  barrier).
-define(TERMINAL,  terminal).
-define(KIOSK,    kiosk).
-define(PRINTER,  printer).
-define(RFID,     rfid).
-define(BLUETOOTH,bluetooth).
-define(BUTTON,   button).
-define(VoIP,     voip).
-define(TOUCH_SCREEN,   touch_screen).
-define(REGISTRATION_SRV,   registration_srv).


-define(DEFAULT_CODE, <<>>).
-define(TTY_QR, "/dev/ttyACM0").
-define(ENC_KEY, "kL~Fv~&<dVRhB3=;veneH( 2AZFZB><^/&TE(w_QW4p,rKAR+p_JZl<#?)o[?").

-define(PAYMENT_REQUEST_CORRELATION_ID, <<"KIOSK_PAYMENT_REQUEST">>).

-define(VALID,    valid).
-define(INVALID,  invalid).
-define(UNDEFINED, undefined).
-define(DB_RES_NOT_FOUND, not_found).
-define(MILLIS, milli_seconds).
-define(OK, ok).
-define(ERROR, error).
-define(TIMEOUT, timeout).
-define(IN_EVENT, <<"IN">>).
-define(OUT_EVENT, <<"OUT">>).

-define(VoIP_PID,       voip_pid_handler).
-define(CAMERA_PID,     cammera_pid_handler).
-define(SCANNER_PID,    scanner_pid_handler).
-define(BUTTON_PID,     button_pid_handler).
-define(RFID_PID,       rfid_pid_handler).
-define(BLUETOOTH_PID,  bluetooth_pid_handler).
-define(BARRIER_PID,    barrier_pid_handler).
-define(TERMINAL_PID,   terminal_pid_handler).
-define(KIOSK_PID,      kiosk_pid_handler).
-define(TOUCH_SCREEN_PID,touch_screen_pid_handler).

-define(ENTITY_TYPE, <<"entityType">>).
-define(HEADER_COMMAND, <<"command">>).
-define(MESSAGE_PAYLOAD,<<"payload">>).

-define(CREATE_COMMAND, <<"CREATE">>).
-define(UPDATE_COMMAND, <<"UPDATE">>).
-define(DELETE_COMMAND, <<"DELETE">>).
-define(GET_COMMAND,    <<"SELECT">>).


-define(OCCUPANCY, <<"OCCUPANCY">>).
-define(RESERVATION_GROUP, <<"RESERVATION_GROUP">>).


-define(STATUS_SUCCESS, 'SUCCESS').
-define(STATUS_ERROR, 'ERROR').
-define(STATUS_NOT_FOUND, 'NOT_FOUND').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          RECORDS                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(PARKING_TABLE,parking).
-define(CAMERA_TABLE, camera).
-define(BARRIER_TABLE,barrier).
-define(DEPS_TABLE,   dependency).
-define(GROUP_TABLE,  group).
-define(BOOKING_TABLE, booking).
-define(VERIFICATION_TABLE, verification).
-define(EVENT_NOTIFICATION_TABLE, notification).
-define(DEVICE_TABLE, device).

-define(RPC_PAYMENT_REQUEST, payment_request_rpc).
-define(RPC_PAYMENT_RESPONSE, payment_response_rpc).
-define(RPC_PAYMENT_STATUS, payment_status_rpc).

-define(PAYMENT_REQUEST,  payment_request).
-define(PAYMENT_RESPONSE, payment_response).
-define(PAYMENT_STATUS,   payment_status).

-define(INIT_JSON, init).

-define(EVENT_KEY, event_key).
-define(EVENT, pt_event).
-define(QRCODE, qr_code).
-define(QR_MESSAGE, qr_message).
-define(SIMULATOR_DEVICE, simulator_device).
-define(SIMULATOR_REQUEST, simulator_request).
-define(SIMULATOR_BUTTON, simulator_button).
-define(SIMULATOR_BARRIER, barrier_loops).
-define(FUNCTIONAL_ALERT, functional_alert).

-define(USER_ACTION, user_action).
-define(ACTION_CMD, action).

-define(FIELD_ID, id).
-define(FIELD_CAMERA_ID, cameraId).
-define(FIELD_TIMEZONE, timezone).
-define(FIELD_SERIAL, serial).
-define(FIELD_MAC, 'MAC').
-define(FIELD_PUBLIC_KEY, publicKey).
-define(FIELD_SECRET_KEY, secretKey).
-define(FIELD_PARKING_ID, parkingId).
-define(FIELD_LANE, lane).
-define(FIELD_BARRIER_ID, barrierId).
-define(KEY, vehicleId).
-define(FIELD_DATE, date).


-define(GetPid(Device),
  case Device of
    ?CAMERA           -> ?CAMERA_PID;
    ?VoIP             -> ?VoIP_PID;
    ?SCANNER          -> ?SCANNER_PID;
    ?BUTTON           -> ?BUTTON_PID;
    ?RFID             -> ?RFID_PID;
    ?BLUETOOTH        -> ?BLUETOOTH_PID;
    ?BARRIER          -> ?BARRIER_PID;
    ?TERMINAL         -> ?TERMINAL_PID;
    ?KIOSK            -> ?KIOSK_PID;
    ?TOUCH_SCREEN     -> ?TOUCH_SCREEN_PID;
    ?REGISTRATION_SRV -> ?REGISTRATION_SRV;
    _ -> ?UNDEFINED
  end).


-define(VALID_CHECK_IN_DEVICE(Device),
  Device == ?BUTTON   orelse
    Device == ?RFID     orelse
    Device == ?SCANNER  orelse
    Device == ?CAMERA   orelse
    Device == ?BLUETOOTH
).

-define(VALID_DEVICE(Device),
  Device == ?BUTTON   orelse
    Device == ?RFID     orelse
    Device == ?SCANNER  orelse
    Device == ?BLUETOOTH orelse
    Device == ?CAMERA
).

-define(VALID_KIOSK_DEVICE(Device),
  Device == ?RFID     orelse
    Device == ?SCANNER  orelse
    Device == ?TOUCH_SCREEN  orelse
    Device == ?BLUETOOTH
).

-define(IS_BLANK(Value),
  Value == null orelse
    Value == <<>> orelse
    Value == <<"">> orelse
    Value == [] orelse
    Value == "" orelse
    Value == ?UNDEFINED).

-define(IS_NOT_BLANK(Value),not ?IS_BLANK(Value)).

-define(MESSAGE(CorrelationID, Payload), <<>>).