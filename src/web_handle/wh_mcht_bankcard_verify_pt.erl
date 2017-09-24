%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 九月 2017 10:34
%%%-------------------------------------------------------------------
-module(wh_mcht_bankcard_verify_pt).
-author("pingjianwei").

%% API
-export([
  init/3
  , handle/2
  , terminate/3

]).


%%-------------------------------------------------------------------
init(_, Req, _Opts) ->
  {Reply, Req2} = xfutils:only_allow(post, Req),
  {Reply, Req2, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, State) ->
  {ok, PostVals, Req2} = xfutils:post_get_qs(Req),

  lager:debug("in /verifying bankcard request qs = ~p", [PostVals]),

  {StatusCode, ReplyBody} = behaviour_verify_process:process(ph_mcht_bankcard_req, PostVals),

  {ok, Req3} = cowboy_req:reply(StatusCode
    , [{<<"content_type">>, <<"application/json">>}]
    , ReplyBody, Req2
  ),

  {ok, Req3, State}.

