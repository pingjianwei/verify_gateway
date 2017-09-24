%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 九月 2017 14:00
%%%-------------------------------------------------------------------
-module(repo_mcht_verify_log_pt).
-author("pingjianwei").

%% API
-export([]).


-compile(export_all).
%%-------------------------------------------------------------
-define(TBL, mcht_verify_log).


-type txn_type() :: bankcard .
-type status() :: success |waiting |fail.
-type txn_status()::status().

-export_type([txn_type/0, status/0]).


-record(?TBL, {
  mcht_index_key
  , txn_type :: txn_type()
%%  the request fields 0f verifying bankcard
  , mcht_id
  , mcht_txn_date
  , mcht_txn_seq
  , mcht_txn_time
  , acct_no
  , acct_name
  , cert_no
  , phone
%% pay fee for verifying bankcard
  , txn_fee
%%  the response result of verifying bankcard
  , txn_result
%%  , quota
  , resp_code
  , resp_msg
  , txn_status = waiting :: txn_status()
}).
-type ?TBL() :: #?TBL{}.
-export_type([?TBL/0]).
-export_records([?TBL]).

%%-------------------------------------------------------------
%% call backs
table_config() ->
  #{
    table_indexes => [tranDate]
    , data_init => []
    , pk_is_sequence => false
    , pk_key_name => mcht_index_key
    , pk_type => tuple

    , unique_index_name => mcht_index_key
    , query_option =>
  #{
    merchId => integer_equal
  }

  }.
