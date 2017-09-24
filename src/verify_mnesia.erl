-module(verify_mnesia).
-include_lib("eunit/include/eunit.hrl").
-include("include/store.hrl").

-export([
  init/0
  , init_table/0
  , start/0
  , db_init/0
]).


init() ->
  {ok, Dir} = application:get_env(mnesia, dir),
  lager:debug("Mnesia dir = ~p", [Dir]),
  %% set path of mnesia
  application:set_env(mnesia, dir, Dir),
  init_table().

init_table() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  mnesia:create_schema([node()]),
  ok = mnesia:start(),
  db_init().

start() ->
  %io:format("Application = ~p~n",[application:get_application()]),
  {ok, Dir} = application:get_env(mnesia, dir),
  lager:debug("Mnesia dir = ~p", [Dir]),
  application:set_env(mnesia, dir, Dir),

  ok = mnesia:start().

%% Internal Functions
db_init() ->
  [
    {db_init(T), index_init(T)}
    || T <- [merchants, mcht_verify_log, ums_verify_log]
  ].

db_init(merchants) ->
  {atomic, ok} = mnesia:create_table(
    merchants,
    [
      {attributes, record_info(fields, merchants)}
      , {disc_copies, [node()]}
    ]),
  data_init();
db_init(mcht_verify_log) ->
  {atomic, ok} = mnesia:create_table(
    mcht_verify_log,
    [
      {attributes, record_info(fields, mcht_verify_log)}
      , {disc_copies, [node()]}
    ]);
db_init(ums_verify_log) ->
  {atomic, ok} = mnesia:create_table(
    ums_verify_log,
    [
      {attributes, record_info(fields, ums_verify_log)}
      , {disc_copies, [node()]}
    ]).


%% add indexes for tabales
index_init(merchants) ->
  {atomic, ok} = mnesia:add_table_index(merchants, mcht_full_name),
  ok;
index_init(mcht_verify_log) ->
  ok;
index_init(ums_verify_log) ->
  ok;
index_init(_) ->
  ok.


data_init() ->
  MerchList = [
    #merchants{
      id = 1
      , mcht_full_name = <<"">>
      , mcht_short_name = <<"">>
      , status = normal
      , txn_fee = 50
      , txn_limit = 500
      , contact = <<"pingjianwei">>
      , phone = 13721422283
      , email = "745241442@qq.com"
      , update_ts = erlang:timestamp()

    }
  ],
  F = fun(X) -> mnesia:dirty_write(X) end,

  lists:foreach(F, MerchList).
