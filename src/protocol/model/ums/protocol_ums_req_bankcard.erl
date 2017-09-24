%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 九月 2017 19:09
%%%-------------------------------------------------------------------
-module(protocol_ums_req_bankcard).
-author("pingjianwei").
-behaviour(behaviour_protocol_model).
-export([sign_fields/0,
  model_type/0,
  model_2_repo_config/0,
  model_2_model_list_config/0,
  model_type/0]).

%% API
-export([]).
-define(TXN, ?MODULE).
-record(?TXN, {

%%  ,merId
  tranId = <<>> :: binary()
  , tranTime = <<>> :: binary()
  , umsAcctType = <<"1">>
  , verifyType = <<>> :: binary()
  , certType = <<"01">>
  , acctNo = <<>> :: binary()
  , acctName = <<>> :: binary()
  , certNo = <<>> :: binary()
  , phone = <<>> :: binary()
  , sign
  , mcht_index_key
}).

-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).


sign_fields() ->
  [
    acctName
    , acctNo
    , certNo
    , certType
    , phone
    , tranId
    , tranTime
    , umsAcctType
    , verifyType
  ].


model_2_repo_config() ->
  [
    {mcht_index_key, mcht_index_key}
    , {tranId, tranId}
    , {tranTime, tranTime}
    , {umsAcctType, umsAcctType}
    , {verifyType, verifyType}
    , {certType, certType}
    , {acctNo, acctNo}
    , {acctName, acctName}
    , {certNo, certNo}
    , {phone, phone}
%%    , {up_index_key, [merchId, txnTime, orderId], tuple}
  ].

model_type() ->
  up.

model_2_model_list_config() ->
  OrderId = xfutils:get_new_order_id(),

  [
    {default,
      [
        {protocol_mcht_req_bankcard, protocol_ums_req_bankcard,
          [
%%            {merId, fun get_mer_id/1, [mcht_id]}
            {tranId, fun get_txn_info/1, [{OrderId, tranId}]}
            , {tranTime, fun get_txn_info/1, [{OrderId, tranTime}]}
            , {verifyType, fun get_verify_type/1, [phone]}
            , {acctNo, acctNo}
            , {acctName, acctName}
            , {certNo, certNo}
            , {phone, phone}
            , {mcht_index_key, mcht_index_key}

          ]
        }
      ]
    }
  ].

get_txn_info({OrderId, Option}) ->
  case Option of
    tranId ->
      OrderId;
    tranTime ->
      <<TranTime:14/bytes, _/binary>> = OrderId,
      TranTime
  end.

get_verify_type(Phone) ->
  case Phone of
    unfined ->
      <<"0030">>;
    _ ->
      <<"0040">>
  end.

get_mer_id(MchtId) ->
  [PaymentMethod] = behaviour_repo:fetch_by(repo_merchants_pt, MchtId, payment_method),
  MerIdAtom = up_config:get_mer_id(PaymentMethod),
  MerIdBin = atom_to_binary(MerIdAtom, utf8).