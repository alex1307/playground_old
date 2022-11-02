%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Aug 2021 16:02
%%%-------------------------------------------------------------------
-author("alex").

-define(MESSAGE_COLLECTOR_SRV, collector_srv).
-define(MESSAGE_COLLECTOR_SUP, collector_sup).

-define(WORKER1_SRV, worker1).
-define(WORKER2_SRV, worker2).
-define(WORKER3_SRV, worker3).
-define(WORKER4_SRV, worker4).
-define(WORKER5_SRV, worker5).
-define(WORKER6_SRV, worker6).

-define(WORKER1_SUP, sup_worker1).
-define(WORKER2_SUP, sup_worker2).
-define(WORKER3_SUP, sup_worker3).
-define(WORKER4_SUP, sup_worker4).
-define(WORKER5_SUP, sup_worker5).
-define(WORKER6_SUP, sup_worker6).

-define(ROOT_LB, root_lb).
-define(ROOT_LB_SUP, sup_lb_root).

-define(INCOMING_MESSAGE_COUNTER_SRV, incoming_counter_srv).
-define(OUTGOING_MESSAGE_COUNTER_SRV, outgoing_counter_srv).
