%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 九月 2017 22:48
%%%-------------------------------------------------------------------
-module(json).
-author("pingjianwei").

%% API
-export([json/0]).

json() ->
%%  ok =  application:start(kernel),
%%  ok = application:start(stdlib),
  UmsBankUrl = "http://218.5.69.214:8088/easserver/gateway/1/realNameVerify/030000563/",
  Body1 = <<"{\"acctName\":\"平建伟\",\"acctNo\":\"6222520623231350\",\"certNo\":\"410183198810141016\",\"certType\":\"01\",\"phone\":\"13721422283\",\"sign\":\"C+78JyLanu2bvVPcnY9QhbNqlm46Lez/pY4UEISkO7npA7dTgboNDHwYwhw412IkzhT0f+2KFpLVud/Zb+IJsDx6ZJ6o2HfpM8DM/IOOCtS8L1kH0eu6mlZYIRgvtmXx5sH0kpue33F2jZdzliSZMeD9Jp0yFasAJGnhnkblPrI=\",\"tranId\":\"20170924222850590579806\",\"tranTime\":\"20170924222850\",\"umsAcctType\":\"1\",\"verifyType\":\"0040\"}"/utf8>>,
  {ok, {{_, RespCode, _}, _, Body2}} = httpc:request(post, {UmsBankUrl, [], "application/json;charset=UTF-8", Body1}, [], []),
  Response = httpc:request(post, {UmsBankUrl, [], "application/json;charset=UTF-8", Body1}, [], []),

  lager:debug("Respnse = ~ts", [Body2]),
  lager:debug("Respnse = ~p", [Response]).