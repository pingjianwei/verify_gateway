%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 2:52 PM
%%%-------------------------------------------------------------------
-module(ph_validate).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  validate_req_fields/3
  , verify_sig/3
  , verify_orig_txn_existed/2
  , dup_id_check/2
  , mcht_id_check/2
  , payment_method_check/2
  , txn_amt_limit_check/2

]).

-compile(export_all).

-type validate_result() :: ok | fail.
-type resp_cd() :: binary().
-type resp_msg() :: binary().

-define(TxnAmtMin, 50).
-define(BankCardNoLenMin, 15).
-define(BankCardNoLenMax, 21).
%%-------------------------------------------------------------------
txn_amt_limit_check(M, Model) ->
  TxnAmt = utils_recop:get(M, Model, mcht_txn_amt),
  %% txm_amt must greater than min value
  MinTxnAmt = get_default_min_txn_amt(),
  try
    true = (MinTxnAmt =< TxnAmt),
    ok
  catch
    _:_ ->
      % check failed
      lager:error("Verify txn amt exceed limit value, TxnAmt = ~p,MinTxnAmt = ~p", [TxnAmt, MinTxnAmt]),
      throw({validate_fail, <<"99">>, <<"交易金额小于限额"/utf8>>})
  end.

txn_amt_limit_check_test() ->
  MchtTxnLog1 = utils_recop:new(repo_mcht_txn_log_pt, [{mcht_txn_amt, ?TxnAmtMin}]),
  ?assertEqual(ok, txn_amt_limit_check(repo_mcht_txn_log_pt, MchtTxnLog1)),

  MchtTxnLog2 = utils_recop:new(repo_mcht_txn_log_pt, [{mcht_txn_amt, ?TxnAmtMin + 1}]),
  ?assertEqual(ok, txn_amt_limit_check(repo_mcht_txn_log_pt, MchtTxnLog2)),

  MchtTxnLog3 = utils_recop:new(repo_mcht_txn_log_pt, [{mcht_txn_amt, ?TxnAmtMin - 1}]),
  ?assertThrow({validate_fail, _, _}, txn_amt_limit_check(repo_mcht_txn_log_pt, MchtTxnLog3)),

  MchtTxnLog4 = utils_recop:new(repo_mcht_txn_log_pt, [{mcht_txn_amt, 0}]),
  ?assertThrow({validate_fail, _, _}, txn_amt_limit_check(repo_mcht_txn_log_pt, MchtTxnLog4)),
  ok.
%%-------------------------------------------------------------------
-spec validate_req_fields(From, Type, Params) -> Result when
  From :: mcht | up,
  Type :: pay | query | refund,
  Params :: proplists:proplist(),
  Result :: {validate_result, validate_result(), resp_cd(), resp_msg()}.

validate_req_fields(From, Type, Params) when is_atom(From), is_atom(Type), is_list(Params) ->
  F = fun({Key, Value}, {ok, _, _} = _AccIn) when is_binary(Key) ->
    try
      validate_one_post_field(From, Key, Value),
      {ok, <<>>, <<>>}
    catch
      _:_ ->
        ErrorMsg = <<Key/binary, "=[", Value/binary, "]格式错误"/utf8>>,
        lager:error("Post vals error = ~ts", [ErrorMsg]),
        {fail, <<"99">>, ErrorMsg}
    end;

    (_, {fail, _, _} = AccIn) ->
      %% previous post kv already validate fail, just pass it throuth
      AccIn
      end,

  {OkOrFail, RespCd, RespMsg} = lists:foldl(F, {ok, <<>>, <<>>}, Params),
  {OkOrFail, RespCd, RespMsg}.


%% for mcht req
validate_one_post_field(mcht, <<"merchId">>, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value);
validate_one_post_field(mcht, <<"tranDate">>, Value) when is_binary(Value) ->
  ok = validate_string(date_yyyymmdd, Value);
validate_one_post_field(mcht, <<"tranId">>, Value) when is_binary(Value) ->
  ok;
validate_one_post_field(mcht, <<"tranTime">>, Value) when is_binary(Value) ->
  6 = byte_size(Value),
  ok = validate_string(integer, Value);
validate_one_post_field(mcht, <<"acctNo">>, Value) when is_binary(Value) ->
  ok ;
validate_one_post_field(mcht, <<"acctName">>, Value) when is_binary(Value) ->
  ok;
validate_one_post_field(mcht, <<"certNo">>, Value) when is_binary(Value) ->
  18 = byte_size(Value),
  ok;
validate_one_post_field(mcht, <<"phone">>, Value) when is_binary(Value) ->
  ok ;
validate_one_post_field(mcht, <<"signature">>, <<>>) ->
  %% orderDesc could not be omit or empty string
  ok = bad;
validate_one_post_field(mcht, <<"signature">>, <<"\"\"">>) ->
  ok = bad;
validate_one_post_field(mcht, <<"signature">>, _) ->
  ok;
validate_one_post_field(mcht, _, _) ->
  ok;
%% for unionpay resp & info
validate_one_post_field(up, _, _) ->
  ok.

validate_string(integer, String) when is_list(String) ->
  validate_string(integer, list_to_binary(String));
validate_string(integer, String) when is_binary(String) ->
  try
    binary_to_integer(String),
    ok
  catch
    _:_ ->
      fail
  end;
validate_string(bank_card_no, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value),
  Len = byte_size(Value),
  true = (?BankCardNoLenMin =< Len) and (?BankCardNoLenMax >= Len),
  ok;
validate_string(txn_amt, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value),
  ok;
validate_string(date_yyyymmdd, Value) when is_binary(Value) ->
%%  8 = byte_size(Value),
  utils_assert:assert(yyyymmdd, Value),
%%  ok = validate_string(integer, Value),
%%  <<Y:4/bytes, M:2/bytes, D:2/bytes>> = Value,
%%  Year = binary_to_integer(Y),
%%  Month = binary_to_integer(M),
%%  Day = binary_to_integer(D),
%%  true = (Year > 2000) and (Year < 2030),
%%  true = (Month > 0) and (Month < 13),
%%  true = (Day > 0) and (Day < 32),
  ok.

validate_one_post_field_test() ->
  ?assertEqual(ok, validate_one_post_field(mcht, <<"tranTime">>, <<"121212">>)),
  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"tranTime">>, <<"u21212">>)),

  ?assertEqual(ok, validate_one_post_field(mcht, <<"tranDate">>, <<"20161010">>)),
%%  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"tranDate">>, <<"201610yy">>)),
  ?assertError(badarg, validate_one_post_field(mcht, <<"tranDate">>, <<"201610yy">>)),
  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"tranDate">>, <<"19991919">>)),

  ?assertEqual(ok, validate_one_post_field(mcht, <<"trustBackUrl">>, <<"http://www.a.b">>)),
  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"trustBackUrl">>, <<"/www.a.b">>)),
  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"trustBackUrl">>, <<"www.a.b">>)),

  ?assertEqual(ok, validate_one_post_field(mcht, <<"trustFrontUrl">>, <<"http://www.a.b">>)),
  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"trustFrontUrl">>, <<"/www.a.b">>)),
  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"trustFrontUrl">>, <<"www.a.b">>)),

  ?assertEqual(ok, validate_one_post_field(mcht, <<"tranAmt">>, <<"100">>)),
  ?assertEqual(ok, validate_one_post_field(mcht, <<"tranAmt">>, <<"50">>)),
%%  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"tranAmt">>, <<"49">>)),
%%  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"tranAmt">>, <<"0">>)),
%%  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"tranAmt">>, <<"-30">>)),

  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"orderDesc">>, <<>>)),
  ?assertError({badmatch, _}, validate_one_post_field(mcht, <<"orderDesc">>, <<"">>)),
  ?assertEqual(ok, validate_one_post_field(mcht, <<"orderDesc">>, <<"xxx">>)),

  ok.
%%-------------------------------------------------------------------
-spec verify_sig(Type, M, Model) -> Result when
  Type :: up|mcht,
  M :: atom(),
  Model :: tuple(),
  Result :: ok.
verify_sig(mcht, M, Model) when is_atom(M), is_tuple(Model) ->
  try
    ok = behaviour_protocol_model:verify(mcht, M, Model)
  catch
    _:X ->
      lager:error("verify mcht sig error . Reason = ~p", [X]),
      throw({validate_fail, <<"11">>, <<"签名验证失败"/utf8>>})
  end;
verify_sig(up, M, Model) when is_atom(M), is_tuple(Model) ->
  try
    ok = behaviour_protocol_model:verify(up, M, Model)
  catch
    _:X ->
      lager:error("verify up sig error . Reason = ~p", [X]),
      throw({validate_fail, <<"11">>, <<"签名验证失败"/utf8>>})
  end.


%%-------------------------------------------------------------------
-spec verify_orig_txn_existed(Type, MchtIndexKey) -> Result when
  Type :: mcht| up,
  MchtIndexKey :: tuple(),
  Result :: ok.

verify_orig_txn_existed(mcht, MchtIndexKey) when is_tuple(MchtIndexKey) ->
  {ok, OrigMchtTxnLog} = behaviour_repo:fetch(repo_mcht_txn_log_pt, MchtIndexKey),

  case OrigMchtTxnLog of
    [] ->
      %% not found
      lager:error("Mcht original txn not found! MchtIndexKey = ~p", [MchtIndexKey]),
      throw({validate_fail, <<"35">>, <<"交易流水号查无原商户交易"/utf8>>});
    _ ->
      ok
  end;
verify_orig_txn_existed(up, MchtIndexKey) when is_tuple(MchtIndexKey) ->
  {ok, OrigUpTxnLog} = behaviour_repo:fetch(repo_up_txn_log_pt, MchtIndexKey),

  case OrigUpTxnLog of
    [] ->
      %% not found
      lager:error("Up original txn not found! MchtIndexKey = ~p", [MchtIndexKey]),
      throw({validate_fail, <<"35">>, <<"交易流水号查无原银联在线交易"/utf8>>});
    _ ->
      ok
  end.
%% =================
-spec dup_id_check(M, Model) -> ok when
  M :: atom(),
  Model :: protocol_mcht_req_pay:protocol_mcht_req_pay().
dup_id_check(M, Model) ->
  PK = utils_recop:get(M, Model, mcht_index_key),
  case behaviour_repo:fetch(repo_mcht_verify_log_pt, PK) of
    {ok, []} ->
      ok;
    {ok, [_Repo]} ->
      throw({validate_fail, <<"12">>, <<"商户交易流水号重复"/utf8>>})
  end.

%% =================
-spec mcht_id_check(M, Model) -> ok when
  M :: atom(),
  Model :: protocol_mcht_req_pay:protocol_mcht_req_pay().

mcht_id_check(M, Model) when is_atom(M) ->
  MchtId = utils_recop:get(M, Model, merchId),

%%  {ok, Mchant} = model_mchants:fetch(binary_to_integer(MchtId)),
  {ok, Mchant} = behaviour_repo:fetch(repo_merchants_pt, MchtId),

  case Mchant of
    [] ->
      % not found mchant id
      throw({validate_fail, <<"31">>, <<"商户号不存在"/utf8>>});
    _ ->
      ok
  end.

%% =================
-spec payment_method_check(M, Model) -> ok when
  M :: atom(),
  Model :: protocol_mcht_req_pay:protocol_mcht_req_pay().

%% check Mchant's payment_method field
%% is payment_method = [gw_netbank_only] , then bank_id value must within
%% unionpay_config's bank list
payment_method_check(M, Model) when is_atom(M) ->
  MchtId = utils_recop:get(M, Model, merchId),
  {ok, [Mchant]} = behaviour_repo:fetch(repo_mchants_pt, MchtId),
  [PaymentMethod] = utils_recop:get(repo_mchants_pt, Mchant, payment_method),
  BankId = utils_recop:get(M, Model, bank_id),
  CardNo = utils_recop:get(M, Model, bank_card_no),

  do_check_payment_method(PaymentMethod, BankId, CardNo).

do_check_payment_method(PaymentMethod, BankId, CardNo) ->
  case up_config:check_payment_method(PaymentMethod, BankId, CardNo) of
    ok ->
      ok;
    error_bank_id_not_allowed ->
      lager:error("Verify payment method error, payment_method = ~p,bank_id = ~p", [PaymentMethod, BankId]),
      throw({validate_fail, <<"32">>, <<"网银无卡通道不允许指定银行直连代码"/utf8>>});
    error_bank_id_error ->
      lager:error("Verify payment method error, payment_method = ~p,bank_id = ~p", [PaymentMethod, BankId]),
      throw({validate_fail, <<"32">>, <<"网银直连银行代码错"/utf8>>});
    error_card_no_not_allowd ->
      lager:error("Verify payment method error, payment_method = ~p,bank_id = ~p,BankCardNo = ~p", [PaymentMethod, BankId, CardNo]),
      throw({validate_fail, <<"32">>, <<"网银直连银行不允许指定银行卡号"/utf8>>})
  end.
%%  do_payment_method_check(PaymentMethod, BankId, {M, Model}).

do_payment_method_check(gw_netbank, Undefined, _)
  when (Undefined =:= <<>>)
  or (Undefined =:= undefined) ->
  ok;
do_payment_method_check(gw_netbank, BankId, _) ->
  %% not allow bank_id for gw_netbank
  %% tempory for minsheng huidong merchant
  %% only allow minsheng
  lager:error("Verify payment method error, payment_method = ~p,bank_id = ~p", [gw_netbank, BankId]),
  throw({validate_fail, <<"32">>, <<"网银无卡通道不允许指定银行直连代码"/utf8>>});
do_payment_method_check(gw_wap, _, _) ->
  ok;
do_payment_method_check(gw_netbank_only, BankId, {M, Model})
  when is_binary(BankId), is_atom(M) ->
  try
%%    ok = gws_up_config:check_bankid(gw_netbank_only, BankId)
    CardNo = utils_recop:get(M, Model, bank_card_no),
    ok = up_config:check_payment_method(gw_netbank_only, BankId, CardNo)
  catch
    _:_ ->
% check failed
      lager:error("Verify payment method error, payment_method = ~p,bank_id = ~p", [gw_netbank_only, BankId]),
      throw({validate_fail, <<"32">>, <<"网银直连银行代码错"/utf8>>})
  end,

  BankCardNo = utils_recop:get(M, Model, bank_card_no),
  case BankCardNo of
    <<>> ->
      ok;
    undefined ->
      ok;
    _ ->
% check failed
      lager:error("Verify payment method error, payment_method = ~p,bank_id = ~p,BankCardNo = ~p", [gw_netbank_only, BankId, BankCardNo]),
      throw({validate_fail, <<"32">>, <<"网银直连银行不允许指定银行卡号"/utf8>>})
  end.
%%-------------------------------------------------------------------
-spec verify_orig_txn_refund(Type, ModelMchtReqRefund) -> Result when
  Type :: mcht| up,
  ModelMchtReqRefund :: tuple(),
  Result :: ok.

verify_orig_txn_refund(mcht, ModelMchtReqRefund) when is_tuple(ModelMchtReqRefund) ->
  try
    OrigMchtIndexKey = utils_recop:get(protocol_mcht_req_refund, ModelMchtReqRefund, orig_mcht_index_key),
    {ok, [RepoOrigMchtTxnLog]} = behaviour_repo:fetch(repo_mcht_txn_log_pt, OrigMchtIndexKey),
    ok = do_verify_refund_vs_orig(ModelMchtReqRefund, RepoOrigMchtTxnLog)
  catch
    throw:{validate_fail, RespCd, RespMsg} ->
      throw({validate_fail, RespCd, RespMsg});
    _:X ->
      lager:error("Verify orig txn refund error = ~p", [X]),
      throw({validate_fail, <<"99">>, <<"验证退款原始交易状态失败"/utf8>>})
  end.

%%  {ok, OrigMchtTxnLog} = behaviour_repo:fetch(repo_mcht_txn_log_pt, MchtIndexKey),

do_verify_refund_vs_orig(ModelMchtReqRefund, RepoOrigMchtTxnLog) when is_tuple(ModelMchtReqRefund), is_tuple(RepoOrigMchtTxnLog) ->
  OrigTxnStatus = utils_recop:get(repo_mcht_txn_log_pt, RepoOrigMchtTxnLog, txn_status),
  case OrigTxnStatus of
    fail ->
      throw({validate_fail, <<"35">>, <<"原支付交易失败，无法退货"/utf8>>});
    waiting ->
      throw({validate_fail, <<"35">>, <<"原支付交易尚未成功，无法退货"/utf8>>});
    success ->
      ok
  end,

  OrigTxnAmt = utils_recop:get(repo_mcht_txn_log_pt, RepoOrigMchtTxnLog, mcht_txn_amt),
  RefundamtThisTime = utils_recop:get(protocol_mcht_req_refund, ModelMchtReqRefund, mcht_txn_amt),
  RefundedSumAmt = sum_all_succ_or_accepted_refund_amt(ModelMchtReqRefund),
  case OrigTxnAmt < (RefundedSumAmt + RefundamtThisTime) of
    false ->
      %% ok
      ok;
    true ->
      %% refund too large this time
      throw({validate_fail, <<"35">>, <<"累计退货金额超过原交易金额"/utf8>>})
  end,
  ok.

%%-------------------------------------------------------------------
sum_all_succ_or_accepted_refund_amt(ModelRefundReq) when is_tuple(ModelRefundReq) ->
  {_, OrigTxnDate, OrigTxnSeq} = utils_recop:get(protocol_mcht_req_refund, ModelRefundReq, orig_mcht_index_key),

  AllRefundReqList = behaviour_repo:query(repo_mcht_txn_log_pt,
    [{filter,
      [{txn_type, refund}
        , {orig_mcht_txn_date, OrigTxnDate}
        , {orig_mcht_txn_seq, OrigTxnSeq}
      ]}]),

  FGatherSuccOrWaitingTxns =
    fun
      (RepoRefund, Acc) ->
        RespCode = utils_recop:get(repo_mcht_txn_log_pt, RepoRefund, resp_code),
        NewAcc = case RespCode of
                   <<"00">> ->
                     [RepoRefund | Acc];
                   <<"05">> ->
                     [RepoRefund | Acc];
                   _ ->
                     Acc
                 end,
        NewAcc
    end,


  FilterRespCodeNot00Or05 = lists:foldl(FGatherSuccOrWaitingTxns, [], AllRefundReqList),

  FSum = fun
           (RepoRefund, Acc) ->
             TxnAmt = utils_recop:get(repo_mcht_txn_log_pt, RepoRefund, mcht_txn_amt),
             Acc + TxnAmt
         end,


  lager:debug("FilteredRefundList = ~p", [FilterRespCodeNot00Or05]),
  SumRefundAmt = lists:foldl(FSum, 0, FilterRespCodeNot00Or05),
  lager:debug("All recorded refund txn amt sum = ~p", [SumRefundAmt]),
  SumRefundAmt.

%%-------------------------------------------------------------------

get_default_min_txn_amt() ->
  MinTxnAmt = case xfutils:get(min_txn_amt) of
                undefined ->
                  ?TxnAmtMin;
                Val ->
                  Val
              end,
  MinTxnAmt.
