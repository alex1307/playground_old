%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2016 2:10 PM
%%%-------------------------------------------------------------------
-module(json_utils).
-author("alex").
-include("mnesia_model.hrl").
%% API

-export([
  set_nth/3,
  index/2,
  index/3]).


set_nth([], Elem, _Position) ->
  [Elem];

set_nth([_H], Elem, 1) ->
  [Elem];

set_nth([_H|T], Elem, 1) ->
  [Elem|T];

set_nth([H|T], Elem, Position) ->
  [H|set_nth(T, Elem, Position -1)].


index(List, Element) ->
  index(List, Element, 1).

index([Element | _Tail], Element, Pos) ->
  Pos;

index([_ | Tail], Element, Pos) ->
  index(Tail, Element, Pos + 1);

index([], _Element, _) ->
  ?UNDEFINED;

index(_Any, _Element, _) ->
  ?ERROR.
