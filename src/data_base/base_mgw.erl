%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Май 2017 10:58
%%%-------------------------------------------------------------------
-module(base_mgw).
-author("rus").

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

%% API
-export([init/0, destroy/0,
    add/5, delete/1,
    get_all/1, get_rec/2]).

%% debug func
%%-export([view]).

%% private record, work only this module!!!
%% name -> "example"
%% mid  -> {ip4Address,{'IP4Address',[192,168,0,81],2944}}
%% {mgw_base,
%%      '192.168.0.143:2944',
%%      {ip4Address,{'IP4Address',[192,168,0,143],2944}},
%%      {ip4Address,{'IP4Address',[192,168,0,81],2944}},
%%      72,
%%      <0.82.0>}
-record(mgw_base, {name, mid_mgw, mid_mgc, count_port, pid}).
-define(TABLE, base_mgw).

-spec(init() ->
    ok | {error, table_all_ready}).
init() ->
    case table_check() of
        false ->
            ets:new(?TABLE, [ordered_set, public, named_table, {keypos, #mgw_base.name}]);
        true ->
            {error, table_all_ready}
    end.

-spec(destroy() ->
    true).
destroy() ->
    try ets:delete(?TABLE) of
        _ -> true
    catch
        _:_ ->
            log:log(warning, "Table \"~p\" no delete~n", [?TABLE]),
            true
    end.

%% Add new record in table
%% Name and Ip is string, Port is integer.
%% Name not check for correct
%% example -> add("example", "192.168.0.81")
%%-spec(add(Name :: tuple(), Ip :: atom(), CountPort :: integer()) ->
%%    ok | {error, Description :: atom()}).
add(Name, MidMGW, MidMGC, CountPort, Pid) ->
    NewRecord = #mgw_base{mid_mgw = MidMGW, mid_mgc = MidMGC, name = Name, count_port = CountPort, pid = Pid},
    case ets:insert_new(?TABLE, NewRecord) of
        true ->
            ok;
        false ->
            {error, name_is_busy}
    end.

%% Remove record in table
%% Name is string.
%% Name not check for correct
%% example -> remove("example")
-spec(delete(Name :: atom()) ->
    true | {error, table_not_create}).
delete(Name) ->
    case table_check() of
        true ->
            ets:delete(?TABLE, Name);
        false ->
            {error, table_not_create}
    end.

-spec(get_all(Name :: string()) ->
    {MID :: {ip4Address, #'IP4Address'{}}, CountPort :: integer()} | empty | {error, table_not_create}).
get_all(Name) ->
    case get_rec(Name) of
        #mgw_base{name = Name, mid_mgw = MidSelf, count_port = CountPort} ->
            {MidSelf, CountPort};
        Res ->
            Res
    end.

-spec(get_rec
    (Name :: atom() | #megaco_conn_handle{}, Field :: conn_handle) -> #megaco_conn_handle{} | unknown;
    (Name :: atom() | #megaco_conn_handle{}, Field :: mid_mgw | mid_mgc) -> {ip4Address, #'IP4Address'{}} | unknown;
    (Name :: atom() | #megaco_conn_handle{}, Field :: ip_self | ip_mgc) -> IP :: #'IP4Address'{} | unknown;
    (Name :: atom() | #megaco_conn_handle{}, Field :: count_port) -> Port :: integer() | unknown).
get_rec(#megaco_conn_handle{local_mid = _MidMGC, remote_mid = MidMGW}, Field) when is_atom(Field) ->
    {ip4Address, #'IP4Address'{address = [A, B, C, D], portNumber = Port}} = MidMGW,
    MgwId = list_to_atom(
        integer_to_list(A, 10) ++ "." ++
        integer_to_list(B, 10) ++ "." ++
        integer_to_list(C, 10) ++ "." ++
        integer_to_list(D, 10) ++ ":" ++
         integer_to_list(Port)),
    get_rec(MgwId, Field);
get_rec(Name, Field) when is_atom(Name) and is_atom(Field) ->
    case get_rec(Name) of
        #mgw_base{name = Name, mid_mgw = MidMGW, mid_mgc = MidMGC, count_port = CountPort, pid = Pid} ->
            case Field of
                conn_handle ->
                    #megaco_conn_handle{local_mid = MidMGC, remote_mid = MidMGW};
                mid_mgc ->
                    MidMGC;
                mid_mgw ->
                    MidMGW;
                ip_self ->
                    {ip4Address, IP} = MidMGW,
                    IP;
                ip_mgc ->
                    {ip4Address, IP} = MidMGC,
                    IP;
                count_port ->
                    CountPort;
                pid ->
                    Pid;
                _ ->
                    unknown
            end;
        Res ->
            Res
    end;
get_rec(_Name, _Field) ->
    unknown.
%%%===================================================================
%%% Private functions
%%%===================================================================

-spec(get_rec(Name :: atom()) ->
    Record :: #mgw_base{} | [] | {error, table_not_create}).
get_rec(Name) when is_atom(Name) ->
    case table_check() of
        true -> case ets:lookup(?TABLE, Name) of
                    [Rec] ->
                        Rec;
                    [] ->
                        []
                end;
        false ->
            {error, table_not_create}
    end;
get_rec(_Name) ->
    {error, bad_argument}.

-spec(table_check() ->
    true | false).
table_check() ->
    case ets:info(?TABLE, named_table) of
        true ->
            true;
        undefined ->
            false
    end.