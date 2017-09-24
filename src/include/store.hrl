%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 九月 2017 15:04
%%%-------------------------------------------------------------------
-author("pingjianwei").
-type txn_type() :: verify.
-type txn_status() :: wating | success | fail.
-type status() :: normal | forizon | closed.
-type name() :: binary().
-type ts() :: erlang:timestamp().
-type id() :: non_neg_integer().


-record(mcht_verify_log, {
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
  , txn_fee = 0
%%  the response result of verifying bankcard
  , txn_result
%%  , quota
  , resp_code
  , resp_msg
  , txn_status = waiting :: txn_status()
}).


-record(merchants, {
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


-record(ums_verify_log, {
  mcht_index_key
  , tranId
  , txnTime
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
