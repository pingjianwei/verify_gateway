%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 九月 2017 15:36
%%%-------------------------------------------------------------------
-module(model_helper).
-author("pingjianwei").

%% API
-export([pretty_model/1]).

pretty_model(ModelList) when is_list(ModelList) ->
  F = fun(Model, Acc) ->
    [pretty_model(Model) | Acc]
      end,
  AccOut = lists:foldl(F, [], ModelList),
  AccOut;

pretty_model(Model) when is_map(Model) ->
  Keys = maps:keys(Model),

  [begin
     case is_atom(Key) of
       true -> {atom_to_binary(Key, utf8), pretty_one_key(Key, Model)};
       false when is_binary(Key) -> {Key, pretty_one_key(Key, Model)}
     end
   end || Key <- Keys].

pretty_one_key(Key, Model)
  when (Key =:= update_ts) or (Key =:= last_update_ts) or (Key =:= last_login_ts) ->

  TS = maps:get(Key, Model),
  TSNew = pretty_ts(TS),
  TSNew;
pretty_one_key(Key, Model) ->
  TS = maps:get(Key, Model),
  pretty_value(TS).

pretty_value(Value) when is_tuple(Value) ->
  tuple_to_list(Value);
pretty_value(Value) ->
  Value.

pretty_ts({_MegaSecs, _Secs, _MicroSec} = TS) ->
  list_to_binary(datetime:now_to_local_string(TS));
pretty_ts(TS) when is_list(TS) ->
  list_to_binary(TS);
pretty_ts(TS) when is_binary(TS) ->
  TS;
pretty_ts(TS) ->
  TS.

