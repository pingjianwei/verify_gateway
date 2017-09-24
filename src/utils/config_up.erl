%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Sep 2016 6:39 PM
%%%-------------------------------------------------------------------
-module(config_up).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([get_key/1]).

-define(APP, payment_gateway).


%%%-------------------------------------------------------------------
-spec get_key(PropName) -> Value when
  PropName :: ums_default_mer_id | up_netbank_mer_id | up_wap_mer_id | atom(),
  Value :: any().

get_key(ums_default_mer_id) ->
  {ok, UmsDefaultMerList} = get_app_key(ums_default_mer_list),
  [NetBankMerId | _] = proplists:get_value(gw_netbank, UmsDefaultMerList),
  {ok, NetBankMerId};
get_key(up_netbank_mer_id_list) ->
  {ok, UpMerList} = get_app_key(up_mer_list),
  UpNetbankMerList = proplists:get_value(gw_netbank, UpMerList),
  {ok, UpNetbankMerList};
get_key(up_wap_mer_id_list) ->
  {ok, UpMerList} = get_app_key(up_mer_list),
  UpWapMerList = proplists:get_value(gw_wap, UpMerList),
  {ok, UpWapMerList};
get_key(Key) ->
  get_app_key(Key).


%%%-------------------------------------------------------------------
%% internal api
-spec get_app_key(Key) -> Value when
  Key :: atom(),
  Value :: any().

get_app_key(Key) ->
  case application:get_env(Key) of
    {ok, Value} -> {ok, Value};
    undefined ->
      application:get_env(?APP, Key)
  end.



