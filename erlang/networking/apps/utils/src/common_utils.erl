
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 21-Aug-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(common_utils).
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-include("constants.hrl").

%% API
-export([get_uuid/0]).


-spec(get_uuid() -> string()).
get_uuid() ->
  uuid:uuid_to_string(uuid:get_v4()).




