%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 九月 2017 10:36
%%%-------------------------------------------------------------------
-module(repo_merchants_pt).
-author("pingjianwei").

-define(BH, behaviour_repo).
%% API
%% callbacks
-export([
  %% table define related
  table_config/0
]).

-compile(export_all).
%%-------------------------------------------------------------
-define(TBL, merchants).

-type ts() :: erlang:timestamp().

-type id() :: non_neg_integer().
-type name() :: binary().
-type status() :: normal | forizon | closed.

-record(?TBL, {
  id = 0 :: id()
  , mcht_full_name = <<"">> :: name()
  , mcht_short_name = <<"">> :: name()
  , status = normal :: status()
  , txn_fee
  , txn_limit
  , contact
  , phone
  , email
  , update_ts = erlang:timestamp() :: ts()
}).
-type ?TBL() :: #?TBL{}.
-export_type([?TBL/0]).
-export_records([?TBL]).
%%-------------------------------------------------------------
%% call backs
table_config() ->
  #{
    table_indexes => [mcht_full_name]
    , data_init => []
    , pk_is_sequence => true
    , pk_key_name => id
    , pk_type => integer

    , unique_index_name => mcht_full_name
    , query_option =>
  #{
    mcht_full_name => within
    , mcht_short_name => within
    , payment_method => member
  }

  }.
