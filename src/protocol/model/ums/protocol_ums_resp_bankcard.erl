%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 九月 2017 20:14
%%%-------------------------------------------------------------------
-module(protocol_ums_resp_bankcard).
-author("pingjianwei").
-define(TXN,?MODULE).

%% API
-export([]).
-record(?TXN, {

  respCode
  , respMsg
  , result
  , sysTranId
  , sysDate
  , tranAmt
  , tranId
  , sign

}).

-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).

sign_fields() ->
  [
    respCode
    , respMsg
    , result
    , sysTranId
    , sysDate
    , tranAmt
    , tranId
  ].