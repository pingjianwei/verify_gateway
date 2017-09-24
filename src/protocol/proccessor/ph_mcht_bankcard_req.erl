%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 九月 2017 11:25
%%%-------------------------------------------------------------------
-module(ph_mcht_bankcard_req).
-author("pingjianwei").

%% API
%% callbacks
-export([
  validate_req/1
  , create_req_model/1
  , validate_req_model/1
  , save_req_model/1
  , create_upstream_req_model/1
  , save_upstream_req_model/1
  , send_verify_upstream_req/1
  , update_upstream_req_record/1
  , render_success_resp_model/1
  , render_fail_resp_model/2
  , render_fail_resp_model/3
]).

-compile(export_all).

%%-------------------------------------------------------------------
validate_req(PostVals) when is_list(PostVals) ->
  ph_validate:validate_req_fields(mcht, verify, PostVals).

create_req_model(PostVals) when is_list(PostVals) ->
  behaviour_protocol_model:out_2_model(protocol_mcht_req_bankcard, PostVals).

validate_req_model(Model) when is_tuple(Model) ->
  % mcht_id check
  ok = ph_validate:mcht_id_check(protocol_mcht_req_bankcard, Model),
  % dup id check
  ok = ph_validate:dup_id_check(protocol_mcht_req_bankcard, Model),
  % sig verify
  ok = ph_validate:verify_sig(mcht, protocol_mcht_req_bankcard, Model),
  %% limit check
  ok = limit_check(protocol_mcht_req_bankcard, Model),
  Model.

save_req_model(Model) when is_tuple(Model) ->
%%  RepoMcht = behaviour_protocol_model:model_2_repo_pk(mcht, protocol_mcht_req_pay, Model),
  RepoMcht = behaviour_protocol_model:save(protocol_mcht_req_bankcard, Model),
%%  ok = behaviour_repo:save(RepoMcht),
  {Model, RepoMcht}.

%% create up req pay model from mcht req model
create_upstream_req_model({Model, RepoMchtTxnLog}) when is_tuple(Model), is_tuple(RepoMchtTxnLog) ->
  lager:info("Model = ~p  ", [Model]),
  ModelUpReqPay = behaviour_protocol_model:model_2_model_list(protocol_ums_req_bankcard, [Model]),

  behaviour_protocol_model:sign(ModelUpReqPay).

%% save up req pay model
save_upstream_req_model(ModelUpReqPay) when is_tuple(ModelUpReqPay) ->
%%  RepoUp = behaviour_protocol_model:model_2_repo_pk(up, protocol_up_req_pay, ModelUpReqPay),
  lager:info("ModelUpReqPay = ~p  ", [ModelUpReqPay]),
  RepoUp = behaviour_protocol_model:save(protocol_ums_req_bankcard, ModelUpReqPay),
  ModelUpReqPay.
send_verify_upstream_req(ModelUpReq) ->

 PropList = behaviour_protocol_model: model_2_out(protocol_ums_req_bankcard, ModelUpReq, proplist),
  PostString = xfutils:post_vals_to_string(PropList),
  ModelUpReqMap = maps:from_list(PropList),
  UmsUrl="http://218.5.69.214:8088/easserver/gateway/1/realNameVerify/{030000563}",
%%  UmsUrl="http://localhost:8888/pg/json",
  lager:debug("ModelUpReqMap = ~p",[ModelUpReqMap]),
  ModelPretty = model_helper:pretty_model([ModelUpReqMap]),
  lager:debug("ModelPretty  = ~p", [ModelPretty]),
  Body = jsx:encode(ModelPretty),
  lager:debug("Body = ~ts", [Body]),
  true = is_binary(Body),
%%  Body1 = binary:replace(Body,[<<"[">>,<<"]">>],<<>>,[global]),
  Body1 ="{\"acctName\":\"平建伟\",\"acctNo\":\"6222520623231350\",\"certNo\":\"410183198810141016\",\"certType\":\"01\",\"phone\":\"13721422283\",\"sign\":\"C+78JyLanu2bvVPcnY9QhbNqlm46Lez/pY4UEISkO7npA7dTgboNDHwYwhw412IkzhT0f+2KFpLVud/Zb+IJsDx6ZJ6o2HfpM8DM/IOOCtS8L1kH0eu6mlZYIRgvtmXx5sH0kpue33F2jZdzliSZMeD9Jp0yFasAJGnhnkblPrI=\",\"tranId\":\"20170924222850590579806\",\"tranTime\":\"20170924222850\",\"umsAcctType\":\"1\",\"verifyType\":\"0040\"}",

  lager:debug("Body1 = ~ts", [Body1]),
  {ok, {{_, RespCode, _}, _, Body2}} = httpc:request(post, {UmsUrl, [], "application/json;charset=UTF-8", Body1}, [], []),
  Respnse = httpc:request(post, {UmsUrl, [], "application/json;charset=UTF-8", Body1}, [], []),
  lager:debug("Respnse = ~ts",[Respnse]),
  lager:debug("UpStreamResp = ~ts",[Body2]),
  ok.
update_upstream_req_record(ModelUpReqPay) ->
  ok
.

%%%% convert up req pay model to post qs
%%convert_resp_model({ModelUpRespPay, _RepoUp}) when is_tuple(ModelUpRespPay) ->
%%  ok.

%% render html @ success , return post form for unionpay pay req
render_success_resp_model(ModelUpRespPay) when is_tuple(ModelUpRespPay) ->
  UpPostVals = utils_recop:to_proplists(protocol_ums_req_bankcard, ModelUpRespPay),
  lager:debug("Render UpPostVals + options = ~p", [UpPostVals]),
  up_pay_dtl:render(UpPostVals).

render_fail_resp_model(RespCd, RespMsg) when is_binary(RespCd), is_binary(RespMsg) ->
  ErrorVals = [{error_code, RespCd}, {error_msg, RespMsg}],
  error_req_dtl:render(ErrorVals).

render_fail_resp_model(RespCd, RespMsg, Model) when is_binary(RespCd), is_binary(RespMsg), is_tuple(Model) ->
  ErrorVals = [{error_code, RespCd}, {error_msg, RespMsg}],
  error_req_dtl:render(ErrorVals).

%% =================
-spec limit_check(M, Model) -> ok when
  M :: atom(),
  Model :: protocol_mcht_req_pay:protocol_mcht_req_pay().
limit_check(M, Model) when is_atom(M) ->
  MchtId = utils_recop:get(M, Model, merchId),
  [Repo] = behaviour_repo:read(repo_merchants_pt, MchtId),
  TxnLimit = utils_recop:get(repo_merchants_pt, Repo, txn_limit),
  TxnFee = utils_recop:get(repo_merchants_pt, Repo, txn_fee),
  case TxnLimit > TxnFee of
    true ->
      ok;
    false ->
      % no quota
%%      {_MchtId, TxnDate, TxnSeq} = utils_recop:get(M, Model, mcht_index_key),
      throw({validate_fail, <<"99">>, <<"余额不足，无法进行验证交易"/utf8>>})
  end.



