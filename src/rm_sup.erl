%%%-------------------------------------------------------------------
%% @doc rm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    NewApp = {app_sup, {app_sup, start_link, []}, permanent, 2000, supervisor, [app_sup]},
    ClientsSup = {clients_sup, {clients_sup, start_link, []}, permanent, 2000, supervisor, [clients_sup]},
    {ok, {{one_for_all, 1, 1}, [NewApp,ClientsSup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
