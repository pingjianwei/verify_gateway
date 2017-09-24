%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 七月 2017 13:10
%%%-------------------------------------------------------------------
-module(utils_misc).
-author("simon").

%% API
-export([
  tc/3
  , tc/4
]).

tc(M, F, A) ->
  Count = 1000,
  tc(M, F, A, Count).

loop_fun(_, 0) ->
  ok;
loop_fun({M, F, A}, Acc) ->
  true = apply(M, F, A),
%%  lager:info("Acc=~p", [Acc]),
  loop_fun({M, F, A}, Acc - 1).

tc(M, F, A, Count) ->
%%  lager:debug("M,F,A = ~p,~p,~p",[M,F,A]),
  L = lists:seq(1, Count),

  Start = erlang:system_time(microsecond),
  loop_fun({M, F, A}, Count),
  End = erlang:system_time(microsecond),

  lager:info("Start = ~p,End = ~p,Ellapsed = ~p", [Start, End, End - Start]),
  lager:info("Every func call consume: = ~p", [(End - Start) / Count]),
  ok.
