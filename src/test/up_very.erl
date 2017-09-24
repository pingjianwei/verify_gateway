%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 七月 2017 15:00
%%%-------------------------------------------------------------------
-module(up_very).
-author("pingjianwei").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-define(SINGSTR, <<"abc">>).

%% API
-export([]).
-compile(export_all).

get_private_key(KeyFileName, Pwd) ->
  {ok, PemBin} = file:read_file(KeyFileName),
  [RSAEntry | _Rest] = public_key:pem_decode(PemBin),
  RsaKeyInfo = public_key:pem_entry_decode(RSAEntry, Pwd),
  RsaKey = public_key:der_decode('RSAPrivateKey', RsaKeyInfo#'PrivateKeyInfo'.privateKey),
  RsaKey.

up_sign(Bin, Key) ->
  S = public_key:sign(Bin, 'sha', Key),
  S.

public_key() ->
  PKFile = "src/keys/acp.pem",
  {ok, PemBin} = file:read_file(PKFile),
  [Certificate] = public_key:pem_decode(PemBin),
  {_, DerCert, _} = Certificate,
  Decoded = public_key:pkix_decode_cert(DerCert, otp),
  PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
  PublicKey.

very() ->
  PrivateKey =get_private_key("priv/keys/ums.key", "111111"),
  lager:info("PrivateKey = ~p~n",[PrivateKey]),
  SignString = "acctName=平建伟&acctNo=6222520623231350&certNo=410183198810141016&certType=01&phone=13721422283&tranId=20170924193055126584290&tranTime=20170924193055&umsAcctType=1&verifyType=0040",
  Signature = up_sign(SignString, PrivateKey),
  lager:info("Signature = ~p~n",[base64:encode(Signature)]),
  PubulicKey = public_key(),
    Flag =public_key:verify(?SINGSTR,sha,Signature,PubulicKey),
  lager:info("result",[Flag]).
