%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% родитель всех MGW
%%% каждый новый MGW добавляется как gen_server
%%% @end
%%% Created : 12. Май 2017 10:33
%%%-------------------------------------------------------------------
-module(mgw_sup).
-author("rus").

-behaviour(supervisor).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([add_mgw/3, add_mgw/4,
    del_mgw/1, del_mgw/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    RestartStrategy = simple_one_for_one, % one_for_one | one_for_all | rest_for_one
    Intensity = 10, %% max restarts
    Period = 1000, %% in period of time
    SupervisorSpecification = {RestartStrategy, Intensity, Period},
    ChildSpecifications = #{id => mgw_gen_fsm,
        start => {mgw_gen_fsm, start_link, []},
        restart => transient, % рестартовать, если он завершился аварийно
        shutdown => 2000,
        type => worker,
        modules => [mgw_gen_fsm]
    },
    log:log(debug, "   Start MGW supervisor with parametrs: ~p~n", [SupervisorSpecification]),
    {ok, {SupervisorSpecification, [ChildSpecifications]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_mgw(MidMGC, Ip, NumPorts) ->
    add_mgw(MidMGC, Ip, ?megaco_ip_port_text, NumPorts).
add_mgw(MidMGC, Ip, Port, NumPorts) ->
    Addr = case inet:parse_address(Ip) of
               {ok, A} ->
                   tuple_to_list(A);
               {error, einval} ->
                   log:log(warning, "ip not correct, set mid user 127.0.0.1~n"),
                   [127, 0, 0, 1]
           end,
    MidMGW = {ip4Address, #'IP4Address'{address = Addr, portNumber = Port}},
    MgwName = list_to_atom(Ip ++ ":" ++ integer_to_list(Port)),
    Res = supervisor:start_child(?SERVER, [MidMGW]),
    log:log(debug, "Count MGW process ~p~n", [supervisor:count_children(?SERVER)]),
    case Res of
        {ok, Pid} ->
            base_mgw:add(MgwName, MidMGW, MidMGC, NumPorts, Pid),
            log:log(notify, "Add MGW [~p,~p]....ok~n", [MgwName, Pid]),
            {ok, MgwName};
        {error, Reason} ->
            log:log(notify, "error: ~p.~n", [Reason]),
            {error, Reason}
    end.

%% return true | []
del_mgw(IdMGW) ->
    case base_mgw:get_rec(IdMGW, pid) of
        Pid when is_pid(Pid) ->
            supervisor:terminate_child(?SERVER, Pid),
            base_mgw:delete(IdMGW);
        _ -> []
    end.
del_mgw(Ip, Port) ->
    IdMGW = list_to_atom(Ip ++ ":" ++ integer_to_list(Port)),
    case base_mgw:get_rec(IdMGW, pid) of
        Pid when is_pid(Pid) ->
            supervisor:terminate_child(?SERVER, Pid),
            base_mgw:delete(IdMGW);
        _ -> []
    end.


