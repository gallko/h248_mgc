%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Сент. 2017 13:46
%%%-------------------------------------------------------------------
-module(mgw).
-author("rus").

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("../include/struct_load.hrl").

-include_lib("stdlib/include/ms_transform.hrl").


%% API
-export([
    add/3, add/4,
    delete/2,

    connection_lost/1,
    connection_established/1,

    work_actions/1
]).

%%%===================================================================
%%% Test functions
%%%===================================================================

-export([test/1]).
test(Key) ->
    MS = ets:fun2ms(fun(#base_mgw_rec{name = Id, count_port = Port}) when Port == Key -> Id end),
    MS.
%%    ets:select(base_mgw, MS).
%%%===================================================================
%%% Public functions
%%%===================================================================

add(MidMGC, Ip, NumPorts) ->
    add(MidMGC, Ip, ?megaco_ip_port_text, NumPorts).
add(MidMGC, Ip, Port, NumPorts) ->
    Addr = case inet:parse_address(Ip) of
               {ok, A} ->
                   tuple_to_list(A);
               {error, einval} ->
                   log:log(warning, "ip not correct, set mid user 127.0.0.1~n"),
                   [127, 0, 0, 1]
           end,
    MidMGW = {ip4Address, #'IP4Address'{address = Addr, portNumber = Port}},
    MgwName = list_to_atom(Ip ++ ":" ++ integer_to_list(Port)),
    ets:insert(base_mgw, #base_mgw_rec{
        name = MgwName,
        mid_mgw = MidMGW,
        mid_mgc = MidMGC,
        count_port = 72,
        status = offline}),
    log:log(notify, "Add MGW [~p]... ok~n", [MgwName]),
    {ok, MgwName}.

%% return ok | []
delete(IdMGW) ->
    case base_mgw:get_rec(IdMGW, conn_handle) of
        unknown ->
            [];
        ConnHandle when is_record(ConnHandle, megaco_conn_handle) ->
            ets:delete(base_mgw, IdMGW),
%%            base_mgw:delete(IdMGW),
            case megaco:disconnect(ConnHandle, delete_adm) of
                ok -> ok;
                {error, _Reason} ->
                    []
            end;
        _ ->
            []
    end.
delete(Ip, Port) when is_list(Ip) and is_integer(Port) ->
    IdMGW = list_to_atom(Ip ++ ":" ++ integer_to_list(Port)),
    delete(IdMGW);
delete(_Ip, _Port) ->
    [].

connection_lost(KeyMG) ->
    case ets:lookup(base_mgw, KeyMG) of
        [Rec | _] ->
            io:format("Connection lost ~p~n", [KeyMG]),
            NewRec = Rec#base_mgw_rec{status = offline, last_access = []},
            ets:insert(base_mgw, NewRec);
        [] ->
            []
    end.

connection_established(KeyMG) ->
    case ets:lookup(base_mgw, KeyMG) of
        [Rec | _] ->
            io:format("Connection established ~p~n", [KeyMG]),
            NewRec = Rec#base_mgw_rec{status = online, last_access = erlang:timestamp()},
            ets:insert(base_mgw, NewRec);
        [] ->
            []
    end.

get_ID_by_pid() ->
    case ets:lookup(base_request, self()) of
        [#base_request_rec{id_mgw = ID} | _] ->
            ID;
        _ ->
            unknown
    end.

work_actions(ActionRequests) ->
    work_actions(ActionRequests, []).

work_actions([ActionRequest | ActionRequests], Ask) ->
    case work_context(ActionRequest) of
        Result when is_record(Result, 'ErrorDescriptor') ->
            {discard_ack, Result};
        [] ->
            ignore_trans_request;
        Result ->
            work_actions(ActionRequests, Ask ++ [Result])
    end;
work_actions([], Ask) ->
    case ets:lookup(base_request, self()) of
        [] ->
            ignore_trans_request;
        [Rec | _] when is_record(Rec, base_request_rec) ->
            case ets:lookup(base_mgw, Rec#base_request_rec.id_mgw) of
                [#base_mgw_rec{status = registration} | _] ->
                    {{handle_ack, registration}, Ask};
                [#base_mgw_rec{status = online} | _] ->
                    {discard_ack, Ask};
                [#base_mgw_rec{status = offline} | _] ->
                    {discard_ack, Ask};
                _ ->
                    ignore_trans_request
            end;
        _ ->
            ignore_trans_request
    end.

work_context(#'ActionRequest'{contextId = ?megaco_null_context_id, commandRequests = Commands}) ->
%%  пробегаемся по все командам в контексте
    case work_commands(Commands) of
        {ok, Ask} ->
%%  TODO    применить команды
            #'ActionReply'{contextId = ?megaco_null_context_id, commandReply = Ask};
        {error, Ask} ->
%%  TODO    отменить команды
            #'ActionReply'{contextId = ?megaco_null_context_id, errorDescriptor = Ask, commandReply = []};
        _ ->
            []
    end;
work_context(#'ActionRequest'{contextId = Ctx, commandRequests = Comands}) ->
%%    принадлежит контексту
    #'ErrorDescriptor'{errorCode = ?megaco_not_implemented};
work_context(#'ActionRequest'{contextId = ?megaco_all_context_id}) ->
    #'ErrorDescriptor'{errorCode = ?megaco_incorrect_identifier, errorText = "*"};
work_context(#'ActionRequest'{contextId = ?megaco_choose_context_id}) ->
    #'ErrorDescriptor'{errorCode = ?megaco_incorrect_identifier, errorText = "$"}.

work_commands(Commands) ->
    work_commands(Commands, []).

work_commands([#'CommandRequest'{command = Command} | Commands], Ask) ->
    case work_command(Command) of
        {ok, Result} ->
            work_commands(Commands, Ask ++ [Result]);
        {error, Error} ->
            {error, Error}
    end;
work_commands([], Ask) ->
    {ok, Ask}.

work_command({serviceChangeReq, #'ServiceChangeRequest'{terminationID = TermID, serviceChangeParms = Params}}) ->
%%  TODO    обработать команду
    ServiceChangeResult = {serviceChangeResParms,
        #'ServiceChangeResParm'{
            serviceChangeVersion = 1
        }
    },
    case TermID of
        [?megaco_root_termination_id] ->
            case get_ID_by_pid() of
                unknown ->
                    {error, #'ErrorDescriptor'{
                        errorCode = ?megaco_internal_gateway_error,
                        errorText = "Pid not found"
                    }};
                ID ->
                    case ets:lookup(base_mgw, ID) of
                        [] ->
                            {error, #'ErrorDescriptor'{
                                errorCode = ?megaco_internal_gateway_error,
                                errorText = "MGW not found in base"
                            }};
                        [Rec | _] ->
                            NewRec = Rec#base_mgw_rec{status = registration},
                            ets:insert(base_mgw, NewRec),
                            {ok, {serviceChangeReply, #'ServiceChangeReply'{
                                terminationID = TermID,
                                serviceChangeResult = ServiceChangeResult
                            }}}
                    end
            end;
        _ ->
            {error, #'ErrorDescriptor'{
                errorCode = ?megaco_not_implemented,
                errorText = "Only ROOT TID"
            }}
    end;
work_command({notifyReq, #'NotifyRequest'{terminationID = _TermID, observedEventsDescriptor = _Event}}) ->
    #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "NotifyRequest"};
work_command(_) ->
    #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "Only ServiceChange or Notify"}.