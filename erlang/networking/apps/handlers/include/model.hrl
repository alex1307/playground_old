
%%%-------------------------------------------------------------------
%%% @author Alexander Todorov(alexander.todorov@ayagasha.com)
%%% @copyright (C) 2020, Ayagasha Enterprises Ltd.
%%% Company Ayagasha Enterprises Ltd.
%%% Created On: 16-Dec-2020
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("Alexander Todorov(alexander.todorov@ayagasha.com)").

-record(avro_handler_state, {
  status::atom,
  schema_dir :: iolist(),
  schemas :: map(),
  encoders :: map(),
  decoders :: map()
}).