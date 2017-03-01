%%%-------------------------------------------------------------------
%% @doc h248_mgc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(h248_mgc_sup).

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Load_MGC = #{id => load_mgc,
        start => {load_mgc, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [load_mgc]
    },
    {ok, { {one_for_all, 0, 1}, [Load_MGC]} }.

%%====================================================================
%% Internal functions
%%====================================================================
