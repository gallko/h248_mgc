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
    log:log(debug, "start link [~p:~p]~n", [?MODULE, ?LINE]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
%%    конфиг
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    Intensity = 10, %% max restarts
    Period = 1000, %% in period of time
    SupervisorSpecification = {RestartStrategy, Intensity, Period},

%%    генсерв первоначальная загрузка
    Load_MGC = #{id => load_mgc,
        start => {load_mgc, start_link, []},
        restart => transient,
        shutdown => 2000,
        type => worker,
        modules => [load_mgc]
    },
%%    супервизор mgw
    Load_MGW = #{id => mgw_sup,
        start => {mgw_sup, start_link, []},
        restart => transient,
        shutdown => infinity,
        type => supervisor,
        modules => [mgw_sup]
    },
    log:log(debug, "   Start MGC supervisor with parametrs: ~p~n", [SupervisorSpecification]),
    {ok, {SupervisorSpecification, [Load_MGC, Load_MGW]}}.

%%====================================================================
%% Internal functions
%%====================================================================
