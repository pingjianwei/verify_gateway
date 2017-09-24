%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 九月 2017 20:14
%%%-------------------------------------------------------------------
-module(protocol_mcht_resp_bankcard).
-author("pingjianwei").
-define(TXN,?MODULE).

%% API
-export([]).

-record(?TXN, {
  result
  , quota
  , respCode
  , respMsg
  , signature
}).
-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).
sign_fields() ->
  [
    result
    , quota
    , respCode
    , respMsg

  ].
