%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Сент. 2017 14:55
%%%-------------------------------------------------------------------
-module(ping_mgw).
-author("rus").

-include("../include/struct_load.hrl").
-include("../include/define_mgc.hrl").
%% API
-export([start/0,
    work/1,
    send_null_ping_mgw/2]).

start() ->
    timer:apply_interval(
        timer:seconds(?PERIOD_ASK_MGW),
        ?MODULE, work, [ping]).

work(ping) ->
    table_traversal(ets:first(base_mgw)).

table_traversal('$end_of_table') ->
    [];
table_traversal(Key) ->
    case ets:lookup(base_mgw, Key) of
        [Record | _] ->
            case Record#base_mgw_rec.status of
                online ->
                    Time = timer:now_diff(erlang:timestamp(), Record#base_mgw_rec.last_access),
                    if Time >= (Record#base_mgw_rec.inactivity_period) ->
                        erlang:spawn(?MODULE, send_null_ping_mgw, [Key, Record]);
                        true -> []
                    end;
                _ ->
                    []
            end;
        _ ->
            []
    end,
    table_traversal(ets:next(base_mgw, Key)).

send_null_ping_mgw(Key, Record) ->
    AuditDesc = #'AuditDescriptor'{},
    Command = {auditValueRequest, #'AuditRequest'{terminationID = ?megaco_root_termination_id, auditDescriptor = AuditDesc}},
    Commands = #'CommandRequest'{command = Command},
    Actions = #'ActionRequest'{contextId = ?megaco_null_context_id, commandRequests = [Commands]},
    ConnHandle = #megaco_conn_handle{local_mid = Record#base_mgw_rec.mid_mgc, remote_mid = Record#base_mgw_rec.mid_mgw},
    Result = megaco:call(ConnHandle, [Actions], [{request_timer, ?TIMER_MEGACO_ASK}]),
    case Result of
        {1, {ok, _Ack}} -> %% update time last_access,
            NewRec = Record#base_mgw_rec{last_access = erlang:timestamp()},
            ets:insert(base_mgw, NewRec);
        {1, {error, timeout}} -> %% lost connection
            erlang:spawn(mgw, connection_lost, [Key]);
        _ -> []
    end.