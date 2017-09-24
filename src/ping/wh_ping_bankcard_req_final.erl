%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 九月 2017 15:57
%%%-------------------------------------------------------------------
-module(wh_ping_bankcard_req_final).
-author("pingjianwei").

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%%-record(state, {}).

init(_, Req, []) ->
  {Reply, Req2} = xfutils:only_allow(post, Req),
  {Reply, Req2, no_state}.

handle(Req, State) ->
  %% get query string
  {ok, PostVals, Req2} = xfutils:post_get_qs(Req),
  lager:info("PostVals = ~p",[PostVals]),
  Model = ph_mcht_bankcard_req:create_req_model(PostVals), % 将请求的参数封装到模型中
  lager:info("Model = ~p",[Model]),
  {SignString,Sign} = behaviour_protocol_model:mcht_sign_msg(protocol_mcht_req_bankcard,Model,protocol_mcht_req_bankcard:sign_fields()),

  MchtOrderVals = convert_key_to_atom(PostVals) ++
    [{signature,Sign},{signString,SignString},{actionUrl,"/verify/bankcard"}],
  lager:debug("~n MchtOrderVals = ~p~n", [MchtOrderVals]),

  %% mcht order parameters passed to dtl
  {ok, Body} = bankcard_verify_final_dtl:render(MchtOrderVals),
  {ok, Req3} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Body, Req),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.


convert_key_to_atom(PropList) when is_list(PropList)->
  F = fun({Key,Value} , Acc)->
       [{erlang:binary_to_atom(Key,latin1),Value}|Acc]
      end,
  lists:foldl(F,[],PropList).

