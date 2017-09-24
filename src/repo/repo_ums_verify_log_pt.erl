%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 九月 2017 14:01
%%%-------------------------------------------------------------------
-module(repo_ums_verify_log_pt).
-author("pingjianwei").

%% API
%% callbacks
-export([
  table_config/0
]).

-compile(export_all).
%%-------------------------------------------------------------
-define(TBL, ums_verify_log).
-type status() :: success |waiting |fail.
-export_type([status/0]).

-record(?TBL, {
  mcht_index_key
  , tranId
  , tranTime
  , umsAcctType
  , verifyType
  , acctNo
  , acctName
  , certNo
  , certType
  , phone
  , up_index_key
  , result
  , sysTranId :: binary()
  , sysDate
  , respCode
  , respMsg

}).

-type ?TBL() :: #?TBL{}.
-export_type([?TBL/0]).

-export_records([?TBL]).
%%-------------------------------------------------------------
%% call backs
table_config() ->
  #{
    table_indexes => [tranId]
    , data_init => []
    , pk_is_sequence => false
    , pk_key_name => mcht_index_key
    , pk_type => tuple

    , unique_index_name => up_index_key
    , query_option =>
  #{
  }

  }.
