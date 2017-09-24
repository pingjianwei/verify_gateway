%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 九月 2017 15:57
%%%-------------------------------------------------------------------
-module(wh_ping_bankcard_req).
-author("pingjianwei").

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, []) ->
  {Reply, Req2} = xfutils:only_allow(get, Req),
  {Reply, Req2, no_state}.

handle(Req, State) ->
  OrderId = xfutils:get_new_order_id(),
  <<Date:8/bytes, Time:6/bytes, _/binary>> = OrderId,

  %% get query string
  MchtOrderVals = [
    {actionUrl, <<"/verify/ping_bankcard_req_final">>}
    , {merchId, "000001"}
    , {tranDate, binary_to_list(Date)}
    , {tranId, binary_to_list(OrderId)}
    , {tranTime,binary_to_list(Time) }
    , {acctNo	, "6222520623231350"}
    , {acctName, <<"平建伟"/utf8>>}
    , {certNo, "410183198810141016"}
    , {phone, "13721422283"}
    , {signature, ""}

  ],
  lager:debug("~n MchtOrderVals = ~p~n", [MchtOrderVals]),

  %% mcht order parameters passed to dtl
  {ok, Body} = bankcard_verify_dtl:render(MchtOrderVals),
  {ok, Req3} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Body, Req),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.

