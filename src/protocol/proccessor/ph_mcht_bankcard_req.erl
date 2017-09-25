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
  PropList = behaviour_protocol_model:model_2_out(protocol_ums_req_bankcard, ModelUpReq, proplist),
%%  ModelUpReqMap = maps:from_list(PropList),
  UmsUrl = "http://218.5.69.214:8088/easserver/gateway/1/realNameVerify/030000563",
  lager:debug("ModelUpReq PropList = ~p", [PropList]),
%%  jsx:encode/1 to convert an erlang term into a utf8 binary containing a json string
  Body = jsx:encode(PropList),
  lager:debug("Up request Body = ~ts", [Body]),
%%  Body1 = binary:replace(Body,[<<"[">>,<<"]">>],<<>>,[global]),
%%  JsonBody = binary:part(Body, 1, byte_size(Body) - 2),
%%  lager:debug("Body1 = ~ts", [JsonBody]),
  {ok, {{_, RespCode, _}, _, Body2}} = httpc:request(post, {UmsUrl, [], "application/json;charset=UTF-8", Body}, [], []),
%%  jsx:decode/1 to convert an erlang term into a utf8 binary containing a json string
  UpRespBody = jsx:decode(list_to_binary(Body2)),
  lager:debug("Up response body = ~p", [UpRespBody]),
  UpRespBody.
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



