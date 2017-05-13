%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Май 2017 17:00
%%%-------------------------------------------------------------------
-module(callback).
-author("rus").

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

%% callback API
-export([
    handle_connect/2,
    handle_disconnect/3,
    handle_syntax_error/3,
    handle_message_error/3,
    handle_trans_request/3,
    handle_trans_long_request/3,
    handle_trans_reply/4,
    handle_trans_ack/4,
    handle_unexpected_trans/3,
    handle_trans_request_abort/4
]).

%%----------------------------------------------------------------------
%% Invoked when a new connection is established
%%----------------------------------------------------------------------

handle_connect(ConnHandle, ProtocolVersion) ->
    log:log(debug, "handle_connect -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "", [ConnHandle, ProtocolVersion]),
    ok.

%%----------------------------------------------------------------------
%% Invoked when a connection is teared down
%%----------------------------------------------------------------------

handle_disconnect(ConnHandle, ProtocolVersion, Reason) ->
    log:log(debug, "handle_disconnect -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "~n   Reason:          ~p"
    "", [ConnHandle, ProtocolVersion, Reason]),
    megaco:cancel(ConnHandle, Reason), % Cancel the outstanding messages
    ok.

%%----------------------------------------------------------------------
%% Invoked when  a received message had syntax errors
%%----------------------------------------------------------------------

handle_syntax_error(ReceiveHandle, ProtocolVersion, ErrorDescriptor) ->
    log:log(debug, "handle_syntax_error -> entry with"
    "~n   ReceiveHandle:   ~p"
    "~n   ProtocolVersion: ~p"
    "~n   ErrorDescriptor: ~p"
    "", [ReceiveHandle, ProtocolVersion, ErrorDescriptor]),
    reply.

%%----------------------------------------------------------------------
%% Invoked when a received message contained no transactions
%%----------------------------------------------------------------------

handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor) ->
    log:log(debug, "handle_message_error -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "~n   ErrorDescriptor: ~p"
    "", [ConnHandle, ProtocolVersion, ErrorDescriptor]),
    no_reply.

%%----------------------------------------------------------------------
%% Invoked for each transaction request
%%----------------------------------------------------------------------

handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests) ->
    log:log(debug, "handle_trans_request -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "~n   ActionRequests:  ~p"
    "~n", [ConnHandle, ProtocolVersion, ActionRequests]),
    case work_ActionRequests(ActionRequests) of
        {ok, Ask} ->
            log:log(debug, "Action reply:  ~p", [Ask]),
            {discard_ack, Ask};
        {error, ErrCode, ErrText} ->
            ErrAsk = #'ErrorDescriptor'{errorCode = ErrCode, errorText = ErrText},
            {discard_ack, ErrAsk};
        _ ->
            ErrAsk = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "In developing."},
            {discard_ack, ErrAsk}
    end.

%%----------------------------------------------------------------------
%% Optionally invoked for a time consuming transaction request
%%----------------------------------------------------------------------

handle_trans_long_request(_ConnHandle, _ProtocolVersion, _ReqData) ->
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
        errorText = "Long transaction requests not handled"},
    {discard_ack, ED}.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction reply
%%----------------------------------------------------------------------

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, ReplyData) ->
    log:log(debug, "handle_trans_eply -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "~n   ActualReply:     ~p"
    "~n   ReplyData:       ~p"
    "", [ConnHandle, ProtocolVersion, ActualReply, ReplyData]),
    ok.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction acknowledgement
%%----------------------------------------------------------------------

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData) ->
    log:log(debug, "handle_trans_ack -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "~n   ckStatus:        ~p"
    "~n   AckData:         ~p"
    "", [ConnHandle, ProtocolVersion, AckStatus, AckData]),
    ok.

%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans) ->
    log:log(debug, "handle_unexpected_trans -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "~n   Trans:           ~p"
    "", [ConnHandle, ProtocolVersion, Trans]),
    ok.


%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_trans_request_abort(_ConnHandle, _ProtocolVersion, _TransId, _Pid) ->
    ok.

%%%===================================================================
%%% Private functions
%%%===================================================================

work_ActionRequests(ActionRequests) ->
%%  Пробегаемся по всем контекстам
    Fun = fun(#'ActionRequest'{contextId = Ctx, commandRequests = CommandRequest}) ->
        work_ActionRequest(Ctx, CommandRequest)
        end,
    Asn = [Fun(A) || A <- ActionRequests],
    {ok, Asn}.

work_ActionRequest(ContextNum, CommandRequest) ->
%%  пробегаемся по все командам в ContextNum
    Fun = fun(Ctx, #'CommandRequest'{command = Comand}) ->
        work_Comand(Ctx, Comand)
        end,
    ReplyCmd = [Fun(ContextNum, Cmd) || Cmd <- CommandRequest],
    #'ActionReply'{contextId = ContextNum, commandReply = ReplyCmd}.

work_Comand(_ContextNum, {serviceChangeReq, #'ServiceChangeRequest'{terminationID = TID, serviceChangeParms = _ServParam}}) ->
    ServiceChangeResult = {serviceChangeResParms, #'ServiceChangeResParm'{}},
    Reply = #'ServiceChangeReply'{terminationID = TID, serviceChangeResult = ServiceChangeResult},
    {serviceChangeReply, Reply}.