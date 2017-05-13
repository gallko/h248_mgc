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
    add/3, add/4, delete/1,
    get_all/1, get_rec/2]).

%% debug func
%%-export([view]).

%% private record, work only this module!!!
%% name -> "example"
%% mid  -> {ip4Address,{'IP4Address',[192,168,0,81],2944}}
-record(mgc_base, {name, mid_self, mid_mgc,count_port}).
-define(TABLE, base_mgc).

-spec(init() ->
    ok | {error, table_all_ready}).
init() ->
    case table_check() of
        false ->
            ets:new(?TABLE, [ordered_set, protected, named_table, {keypos, #mgc_base.name}]);
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
-spec(add(Name :: string(), Ip :: string(), CountPort :: integer()) ->
    ok | {error, Description :: atom()}).
add(Name, Ip, CountPort) ->
    add(Name, Ip, ?megaco_ip_port_text, CountPort).

-spec(add(Name :: string(), Ip :: string(), Port :: integer(), CountPort :: integer()) ->
    ok | {error, name_is_busy} | {error, table_not_create} | {error, incorrect} | {error, ip_incorrect}).
add(Name, Ip, Port, CountPort) when
    is_list(Name) and is_list(Ip) and is_integer(Port) and is_integer(CountPort) ->
    case inet:parse_address(Ip) of
        {ok, A} ->
            MidSelf = {ip4Address, #'IP4Address'{address = tuple_to_list(A), portNumber = Port}},
            NewRecord = #mgc_base{name = Name, mid_self = MidSelf, count_port = CountPort},
            case table_check() of
                true ->
                    case ets:insert_new(?TABLE, NewRecord) of
                        true ->
                            ok;
                        false ->
                            {error, name_is_busy}
                    end;
                false ->
                    {error, table_not_create}
            end;
        {error, einval} ->
            {error, ip_incorrect}
    end;
add(_Name, _Ip, _Port, _CountPort) ->
    {error, incorrect}.

%% Remove record in table
%% Name is string.
%% Name not check for correct
%% example -> remove("example")
-spec(delete(Name :: string()) ->
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
        #mgc_base{name = Name, mid_self = MidSelf, count_port = CountPort} ->
            {MidSelf, CountPort};
        Res ->
            Res
    end.

-spec(get_rec
    (Name :: string(), Field :: conn_handle) -> #megaco_conn_handle{} | unknown;
    (Name :: string(), Field :: mid_self | mid_mgc) -> {ip4Address, #'IP4Address'{}} | unknown;
    (Name :: string(), Field :: ip_self | ip_mgc) -> IP :: #'IP4Address'{} | unknown;
    (Name :: string(), Field :: count_port) -> Port :: integer() | unknown).
get_rec(Name, Field) ->
    case get_rec(Name) of
        #mgc_base{name = Name, mid_self = MidSelf, mid_mgc = MidMGC, count_port = CountPort} ->
            case Field of
                conn_handle ->
                    #megaco_conn_handle{local_mid = MidMGC, remote_mid = MidSelf};
                mid_mgc ->
                    MidMGC;
                mid_self ->
                    MidSelf;
                ip_self ->
                    {ip4Address, IP} = MidSelf,
                    IP;
                ip_mgc ->
                    {ip4Address, IP} = MidMGC,
                    IP;
                count_port ->
                    CountPort;
                _ ->
                    unknown
            end;
        Res ->
            Res
    end.

%%%===================================================================
%%% Private functions
%%%===================================================================

-spec(get_rec(Name :: string()) ->
    Record :: #mgc_base{} | [] | {error, table_not_create}).
get_rec(Name) ->
    case table_check() of
        true -> case ets:lookup(?TABLE, Name) of
                    [Rec] ->
                        Rec;
                    [] ->
                        []
                end;
        false ->
            {error, table_not_create}
    end.

-spec(table_check() ->
    true | false).
table_check() ->
    case ets:info(?TABLE, named_table) of
        true ->
            true;
        undefined ->
            false
    end.