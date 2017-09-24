%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc 封装发往银联在线的对账等操作接口，非交易类的
%%%
%%% @end
%%% Created : 03. 十一月 2016 22:05
%%%-------------------------------------------------------------------
-module(utils_up_cmd).
-include("../include/type_binarys.hrl").
-include("../include/type_up_protocol.hrl").
-author("simon").

%% API
-export([
  reconcile/2
  , reconcile_result_process/2
  , base64_content_inflate/1
]).

-record(state, {txn_type, txn_params, count}).

-define(DEFAULT_QUERY_RETRY_TIMEOUT, 1000 * 5).
-define(DEFAULT_QUERY_RETRY_COUNT, 5).
%%-define(UP_QUERY_URL, "https://gateway.95516.com/gateway/api/queryTrans.do").
%%-define(UP_FILE_URL, "https://filedownload.95516.com/").
-define(UP_QUERY_URL, xfutils:get_path(up_config,up_query_url)).
-define(UP_FILE_URL, xfutils:get_path(up_config,up_reconcile_download_url)).

%%========================================================================
-spec reconcile(SettleDate, MerId) -> PostResult when
  SettleDate :: byte8,
  MerId :: up_merId(),
  PostResult :: {neg_integer(), any(), any()}.

reconcile(SettleDate, MerId) when is_binary(SettleDate), is_binary(MerId) ->
  4 = byte_size(SettleDate),
  15 = byte_size(MerId),


  PostString = txn_post_string(model_up_req_file_transfer, [MerId, SettleDate]),
  {StatusCode, Header, Body} = xfutils:post(?UP_FILE_URL, PostString).


%%========================================================================
txn_post_string(Module, NewArgs) when is_atom(Module), is_list(NewArgs) ->
  lager:debug("Do txn_post_string. Module = ~p,NewArgs = ~p", [Module, NewArgs]),
  {ok, Model} = apply(Module, new, NewArgs),
  PostString = Module:to_post(Model, post_body_string),
  PostString.
%
%%========================================================================
reconcile_result_process(Header, Body) when is_list(Body) ->
  lager:debug("In reconcile_result_process...", []),
  PL = xfutils:parse_post_body(Body),
  RespCode = proplists:get_value(<<"respCode">>, PL),
  case RespCode of
    <<"98">> ->
      % file not exist
      MerId = proplists:get_value(<<"merId">>, PL),
      SettleDate = proplists:get_value(<<"settleDate">>, PL),
      lager:info("Reconcile file not exist! MerId = ~ts,Date = ~ts", [MerId, SettleDate]),
      ok;
    <<"00">> ->
      FileName = proplists:get_value(<<"fileName">>, PL),
      FileContent = proplists:get_value(<<"fileContent">>, PL),
      Ret = base64_content_inflate(FileContent),
      lager:debug("Ready to write reconcile file = ~p", [FileName]),
      gw_up_reconcile:save_up_reconcile_file(FileName, Ret)
  end,
  ok.
%%========================================================================
base64_content_inflate(Content) when is_binary(Content) ->
  Z = zlib:open(),
  zlib:inflateInit(Z),
  Bin = base64:decode(Content),
  lager:debug("Content = ~p", [Content]),
  lager:debug("ContentBin = ~p", [Bin]),
  PKiolist = zlib:inflate(Z, Bin),
  lager:debug("Inflate Result = ~p", [PKiolist]),
  zlib:inflateEnd(Z),
  %[C] = Ret,
  %C.
  PKiolist.
%%========================================================================
%%========================================================================

