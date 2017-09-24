%%%-------------------------------------------------------------------
%% @doc verify_gateway top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(verify_gateway_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MVERIFYENV, verify_enc).
-define(MVERIFYWEB, verify_web).
%%-define(MVERIFYMNESIA, verify_mnesia).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  VeerifyEnv = {?MVERIFYENV,
    {?MVERIFYENV, start_link, []},
    permanent, 2000, worker, [?MVERIFYENV]},
  VerifyWeb = {?MVERIFYWEB,
    {?MVERIFYWEB, start_link, []},
    permanent, 2000, worker, [?MVERIFYWEB]},
%%  VerifyMnesia = {?MVERIFYMNESIA,
%%    {?MVERIFYMNESIA, start_link, []},
%%    permanent, 2000, worker, [?MVERIFYMNESIA]},

  Children = [
    VeerifyEnv,
    VerifyWeb
%%    VerifyMnesia
  ],
  RestartStrategy = {one_for_one, 4, 60},
  {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
