%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 九月 2017 19:09
%%%-------------------------------------------------------------------
-module(protocol_mcht_req_bankcard).
-include("../../../include/type_mcht_protocol.hrl").
-include("../../../include/type_binarys.hrl").
-author("pingjianwei").

%% API

-export([sign_fields/0
  , model_type/0
  , model_2_repo_config/0]).
-define(TXN, ?MODULE).

-record(?TXN, {

  merchId = 9999 :: mcht_id()
  , tranDate = <<>> :: binary()
  , tranId = <<>> :: binary()
  , tranTime = <<>> :: binary()
  , acctNo = <<>> :: binary()
  , acctName = <<>> :: binary()
  , certNo = <<>> :: binary()
  , phone = <<>> :: binary()
  , signature
  , txn_type = verify
  , txn_status = waiting :: txn_status()

}).

-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).

-behaviour(behaviour_protocol_model).


sign_fields() ->

  [
    merchId
    , tranDate
    , tranId
    , tranTime
    , acctNo
    , acctName
    , certNo
    , phone
  ].

model_type() ->
  mcht.


model_2_repo_config() ->

  [
    {txn_type, {static, verify}}
    , {mcht_id, merchId}
    , {mcht_txn_date, tranDate}
    , {mcht_txn_time, tranTime}
    , {mcht_txn_seq, tranId}
    , {acct_no, acctNo}
    , {acct_name, acctName}
    , {cert_no, certNo}
    , {phone, phone}
  ].
