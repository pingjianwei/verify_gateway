%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc 处理加密/签名的进程
%%%
%%% @end
%%% Created : 10. Apr 2016 15:53
%%%-------------------------------------------------------------------
-module(verify_enc).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-author("simonxu").

-behaviour(gen_server).

%% API
-export([start_link/0,
  public_key_file/2,
  private_key/2,
  sign/3,
  sign_fun/2,
  verify/4,
  sign_hex/3,
  sign_hex_fun/2,
  verify_hex/4,
  save_mcht_pk_file/2,
  reload_keys/0,
  perf_tc/0,

  save_mcht_req_pk_file_raw/2
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
%%-define(APP, payment_gateway).
-define(APP, verify_gateway).

-record(state, {mcht_keys}).

-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

public_key_file(MchtId, Direction) ->
  gen_server:call(?SERVER, {public_key_file, MchtId, Direction}).

private_key(MchtId, Direction) ->
  gen_server:call(?SERVER, {private_key, MchtId, Direction}).

sign(MchtId, Direction, SignString) ->
  gen_server:call(?SERVER, {sign, MchtId, Direction, SignString}).

sign_fun(MchtId, Direction) ->
  gen_server:call(?SERVER, {sign_fun, MchtId, Direction}).

verify(MchtId, Direction, DigestBin, SignString) ->
  gen_server:call(?SERVER, {verify, MchtId, Direction, DigestBin, SignString}).

sign_hex(MchtId, Direction, SignString) ->
  gen_server:call(?SERVER, {sign_hex, MchtId, Direction, SignString}).

sign_hex_fun(MchtId, Direction) ->
  gen_server:call(?SERVER, {sign_hex_fun, MchtId, Direction}).

verify_hex(MchtId, Direction, DigestBin, SignString) when
  is_integer(MchtId),
  is_binary(DigestBin),
  is_binary(SignString),
  is_atom(Direction) ->
  gen_server:call(?SERVER, {verify_hex, MchtId, Direction, DigestBin, SignString}).

save_mcht_pk_file(MchtId, ReqPK) when
  is_integer(MchtId),
  is_binary(ReqPK) ->
  gen_server:call(?SERVER, {save_mcht_pk_file, MchtId, ReqPK, rsa_key}).

save_mcht_req_pk_file_raw(MchtId, ReqPK) when
  is_integer(MchtId),
  is_binary(ReqPK) ->
  gen_server:call(?SERVER, {save_mcht_pk_file, MchtId, ReqPK, raw}).

reload_keys() ->
  gen_server:call(?SERVER, {reload_keys}).


perf_tc() ->
  F = sign_fun(1, req),
  L = [$a || _X <- lists:seq(1, 300)],
  S = list_to_binary(L),
  lager:info("ts=~p", [datetime_x_fin:now()]),
  %[ sign(1,req,<<"hello">>) || _X <- lists:seq(1,10000)],
  %lager:info("after sign, ts1=~p~n",[xfutils:now()]),
  %[ F(<<"hello">>) || _X <- lists:seq(1,10000)],
  perf(10000, F, S),
  lager:info("after sign_fun, ts1=~p", [datetime_x_fin:now()]),
  ok.

perf(0, _, _) ->
  ok;
perf(N, F, S) ->
  F(S),
  perf(N - 1, F, S).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  %% 读取商户的相关秘钥
  Dict = load_mcht_keys(),
  {ok, #state{mcht_keys = Dict}}.

load_mcht_keys() ->
  %% 定位商户秘钥所在目录
  PrivDir = mcht_keys_dir(),

  %% 处理目录下所有的商户
  Keys = [mcht_all_keys([PrivDir, Dir], Dir)
    || Dir <- mcht_dirs(PrivDir), lists:nth(1, Dir) =/= $.
  ],
  %lager:debug("~p", [Keys]),
  KeysDict = dict:from_list(lists:flatten(Keys)),
  KeysDict.


mcht_keys_dir() ->
%%  xfutils:get_path/1默认情况下会到payment_gateway应用下获取参数，目前应用名为verify_gateway，
%%  该方法步使用,应该用xfutils:get_path/2
%%  xfutils:get_path([home, priv_dir, mcht_keys_dir]).
  xfutils:get_path(?APP,[home, priv_dir, mcht_keys_dir]).


mcht_dirs(Dir) ->
  {ok, MchtDirList} = file:list_dir_all(Dir),
  MchtDirList.

mcht_all_keys(Dir, MchtId) ->
  %% req: private_key, public_key
  ReqKeys = mcht_one_keypair([Dir, "/req"]),
  %lager:debug("ReqKeys = ~p", [ReqKeys]),


  %% resp: private_key , public_key
  RespKeys = mcht_one_keypair([Dir, "/resp"]),

  lager:info("load mcht [~p] keys ok!", [MchtId]),

  [{{list_to_integer(MchtId), req}, ReqKeys}, {{list_to_integer(MchtId), resp}, RespKeys}].


digest(Bin) ->
  DigestBin = crypto:hash(sha, Bin),
  DigestHexUpper = xfutils:bin_to_hex(DigestBin),
  list_to_binary(string:to_lower(binary_to_list(DigestHexUpper))).


sign_internal(DigestBin, PrivateKey) ->
  Digest = digest(DigestBin),
  Signed = public_key:sign(Digest, sha, PrivateKey),
  Signed.
%base64:encode(Signed).
sign_internal_nodigest(DigestBin, PrivateKey) ->
  %Digest = digest(DigestBin),
  Digest = DigestBin,
  Signed = public_key:sign(Digest, sha, PrivateKey),
  Signed.

sign_hex(DigestBin, PrivateKey) ->
  SignedBin = sign_internal_nodigest(DigestBin, PrivateKey),
  Hex = xfutils:bin_to_hex(SignedBin),
  Hex.

sign64(DigestBin, PrivateKey) ->
  base64:encode(sign_internal(DigestBin, PrivateKey)).



verify_internal(DigestBin, Signature64, PublicKey) ->
  Digested = digest(DigestBin),
  Signature = base64:decode(Signature64),
  public_key:verify(Digested, sha, Signature, PublicKey).

verify_internal_nodigest(DigestBin, Signature64, PublicKey) ->
  %Digested = digest(DigestBin),
  Digested = DigestBin,
  Signature = base64:decode(Signature64),
  public_key:verify(Digested, sha, Signature, PublicKey).

verify_hex(DigestBin, SignatureHex, PublicKey) ->
  %Digested = digest(DigestBin),
  Digested = DigestBin,
  Signature = xfutils:hex_to_bin(SignatureHex),
  public_key:verify(Digested, sha, Signature, PublicKey).



mcht_one_keypair(Dir) ->
%% private_key
  PrivateKeyFileName = [Dir, "/private_key.pem"],
  %lager:debug("private key file = ~p", [PrivateKeyFileName]),
  PrivateKey = get_private_key(PrivateKeyFileName, ""),


%% public_key
  PublicKeyFileName = [Dir, "/public_key.pem"],
  %lager:debug("public key file = ~p", [PublicKeyFileName]),
  PublicKey = get_public_key(PublicKeyFileName),

  {PrivateKey, PublicKey}.



get_private_key(KeyFileName, Pwd) ->
  try
    {ok, PemBin} = file:read_file(KeyFileName),
    [RSAEntry | _Rest] = public_key:pem_decode(PemBin),
    RsaKeyInfo = public_key:pem_entry_decode(RSAEntry, Pwd),
    %lager:info("private key = ~p~n", [RsaKeyInfo]),
    %lager:debug("private key = ~p", [RsaKeyInfo]),
    %RsaKey = public_key:der_decode('RSAPrivateKey', RsaKeyInfo#'PrivateKeyInfo'.privateKey),
    %RsaKey.
    {RsaKeyInfo, PemBin}

  catch
    error :X ->
      lager:error("read private key file ~p error! Msg = ~p", [KeyFileName, X]),
      {<<>>, <<>>}
  end.


get_public_key(KeyFileName) ->
  try
    {ok, PemBin} = file:read_file(KeyFileName),
    [Certificate] = public_key:pem_decode(PemBin),
    %{_, DerCert, _} = Certificate,
    %Decoded = public_key:pkix_decode_cert(DerCert, otp),
    %PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    %PublicKey.
    PublicKey = public_key:pem_entry_decode(Certificate),
    %lager:info("public key = ~p~n", [PublicKey]),
    %lager:debug("public key = ~p", [PublicKey]),
    {PublicKey, PemBin}
  catch
    error:X ->
      lager:error("read public key file ~p error! Msg = ~p", [KeyFileName, X]),
      {<<>>, <<>>}

  end.

get_public_key_raw(KeyFileName) ->
  {ok, PemBin} = file:read_file(KeyFileName),
  {S1, L1} = binary:match(PemBin, <<"-----BEGIN PUBLIC KEY-----">>),
  <<_:S1/bytes, _:L1/bytes, Rest/binary>> = PemBin,
  lager:debug("Rest = ~p", [Rest]),
  {S2, _L2} = binary:match(Rest, <<"---">>),
  <<Raw:S2/bytes, _/binary>> = Rest,
  lager:debug("Raw = ~p", [Raw]),
  RawWOCR = binary:replace(Raw, <<"\n">>, <<>>, [global]),
  lager:debug("RawWOCR = ~p", [RawWOCR]),
  RespPKDecoded = base64:decode(RawWOCR),
  RespPKHex = xfutils:bin_to_hex(RespPKDecoded),
  lager:debug("RespPKHex = ~p", [RespPKHex]),

  RespPKHex.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({sign_hex, MchtId, Direction, DigestBin},
    _From, #state{mcht_keys = KeyDict} = State) ->
  %%{PrivateKey, _} = dict:fetch({MchtId, Direction}, KeyDict),
  PrivateKey = get_private_key_from_state(MchtId, Direction, KeyDict),
  %Signed64 = sign64(DigestBin, PrivateKey),
  SignedHex = sign_hex(DigestBin, PrivateKey),
  {reply, SignedHex, State};

handle_call({sign_hex_fun, MchtId, Direction},
    _From, #state{mcht_keys = KeyDict} = State) ->
  %%{PrivateKey, _} = dict:fetch({MchtId, Direction}, KeyDict),
  PrivateKey = get_private_key_from_state(MchtId, Direction, KeyDict),
  F = fun(DigestBin) ->
    %Signed64 = sign64(DigestBin, PrivateKey),
    %Signed64
    SignedHex = sign_hex(DigestBin, PrivateKey),
    SignedHex
      end,
  {reply, F, State};
handle_call({verify_hex, MchtId, Direction, DigestBin, Signature64},
    _From, #state{mcht_keys = KeyDict} = State) ->
  %%{_, PublicKey} = dict:fetch({MchtId, Direction}, KeyDict),
  PublicKey = get_public_key_from_state(MchtId, Direction, KeyDict),
  Verified = verify_hex(DigestBin, Signature64, PublicKey),
  {reply, Verified, State};

handle_call({sign, MchtId, Direction, DigestBin},
    _From, #state{mcht_keys = KeyDict} = State) ->
  %% {PrivateKey, _} = dict:fetch({MchtId, Direction}, KeyDict),
  PrivateKey = get_private_key_from_state(MchtId, Direction, KeyDict),
  Signed64 = sign64(DigestBin, PrivateKey),
  {reply, Signed64, State};

handle_call({sign_fun, MchtId, Direction},
    _From, #state{mcht_keys = KeyDict} = State) ->
  %%{PrivateKey, _} = dict:fetch({MchtId, Direction}, KeyDict),
  PrivateKey = get_private_key_from_state(MchtId, Direction, KeyDict),
  F = fun(DigestBin) ->
    Signed64 = sign64(DigestBin, PrivateKey),
    Signed64
      end,
  {reply, F, State};
handle_call({verify, MchtId, Direction, DigestBin, Signature64},
    _From, #state{mcht_keys = KeyDict} = State) ->
  %%{_, PublicKey} = dict:fetch({MchtId, Direction}, KeyDict),
  PublicKey = get_public_key_from_state(MchtId, Direction, KeyDict),
  Verified = verify_internal(DigestBin, Signature64, PublicKey),
  {reply, Verified, State};
handle_call({save_mcht_pk_file, MchtId, ReqPK, Option}, _From, State) when is_atom(Option) ->
  PrivDir = mcht_keys_dir(),
  StrMchtId = integer_to_binary(MchtId),
  %% copy from mcht 0
  copy_keys_dir_from_mcht_0(MchtId, PrivDir),


  %% write pk file
  %% trust-one platform send pk in "rsa public key" format , not erlang "public key" format
  write_mcht_req_pk(PrivDir, StrMchtId, ReqPK, Option),

  %% get resp public key
  %% get public key from pkcs#1 format pem file, not the one in pkcs#8 format one
  RespPemFileName = list_to_binary([PrivDir, "/", StrMchtId, "/resp/public_key.pem.pkcs1"]),
  lager:debug("RespPemFileName=~p", [RespPemFileName]),
  RespPK = utils_enc:get_public_key_raw_pkcs1(RespPemFileName),
  lager:debug("RespPK = ~p", [RespPK]),

  {reply, RespPK, State};

handle_call({reload_keys}, _From, _State) ->
  Dict = load_mcht_keys(),
  {reply, ok, #state{mcht_keys = Dict}};


handle_call({public_key_file, MchtId, Direction}, _From, #state{mcht_keys = KeyDict} = State) ->
  PublicKeyFile = get_public_key_file_from_state(MchtId, Direction, KeyDict),
  {reply, PublicKeyFile, State};


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_private_key_from_state(MchtId, Direction, Dict)
  when is_integer(MchtId), is_atom(Direction),
  ((Direction =:= resp) or (Direction =:= req)) ->
  {{PrivateKey, _PrivateKeyFile}, _} = fetch_from_state({MchtId, Direction}, Dict),
  PrivateKey.

get_private_key_file_from_state(MchtId, Direction, Dict)
  when is_integer(MchtId), is_atom(Direction),
  ((Direction =:= resp) or (Direction =:= req)) ->
  {{_PrivateKey, PrivateKeyFile}, _} = fetch_from_state({MchtId, Direction}, Dict),
  PrivateKeyFile.

get_public_key_from_state(MchtId, Direction, Dict)
  when is_integer(MchtId), is_atom(Direction),
  ((Direction =:= resp) or (Direction =:= req)) ->
  {_, {PublicKey, _PublicKeyFile}} = fetch_from_state({MchtId, Direction}, Dict),
  PublicKey.

get_public_key_file_from_state(MchtId, Direction, Dict)
  when is_integer(MchtId), is_atom(Direction),
  ((Direction =:= resp) or (Direction =:= req)) ->
  {_, {_PublicKey, PublicKeyFile}} = fetch_from_state({MchtId, Direction}, Dict),
  PublicKeyFile.

fetch_from_state(Key, Dict) ->
  try
    dict:fetch(Key, Dict)
  catch
    _:_ ->
      lager:error("Could not find key in state. Key = ~p", [Key]),
      No = <<"no content">>,
      {{No, No}, {No, No}}
  end.

copy_keys_dir_from_mcht_0(MchtId, PrivDir) when is_integer(MchtId) ->
  StrMchtId = integer_to_binary(MchtId),
  Cmd = ["cd ", PrivDir, " ;",
    "rm -rf ", StrMchtId, " ;",
    "mkdir ", StrMchtId, ";",
    "cp -r 0/* ", StrMchtId
  ],
  Cmd1 = binary_to_list(list_to_binary(Cmd)),
  lager:debug("Cmd1= ~p", [Cmd1]),
  os:cmd(Cmd1).
copy_keys_dir_from_mcht_1(MchtId, PrivDir) when is_integer(MchtId) ->
  StrMchtId = integer_to_binary(MchtId),
  Cmd = ["cd ", PrivDir, " ;",
    "rm -rf ", StrMchtId, " ;",
    "mkdir ", StrMchtId, ";",
    "cp -r 1/* ", StrMchtId
  ],
  Cmd1 = binary_to_list(list_to_binary(Cmd)),
  lager:debug("Cmd1= ~p", [Cmd1]),
  os:cmd(Cmd1).

write_mcht_req_pk(PrivDir, StrMchtId, ReqPK, Option) ->
  ReqPKPemFileName = list_to_binary([PrivDir, "/", StrMchtId, "/req/public_key.pem"]),
  BinPemPK = case Option of
               rsa_key ->
                 xfutils:bin_to_pem_rsa(ReqPK);
               raw ->
                 ReqPK
             end,
  lager:debug("FileName=~p,BinPemPK = ~p", [ReqPKPemFileName, BinPemPK]),
  ok = file:write_file(ReqPKPemFileName, BinPemPK),
  lager:info("Write req pem file success !", []).

%%%===================================================================


