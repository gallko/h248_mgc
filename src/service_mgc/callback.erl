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
-include("../include/struct_load.hrl").

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
    "~n", [ConnHandle, ProtocolVersion]),
    case connHandle_in_MgwId(ConnHandle) of
        unknown ->
            error;
        ID ->
            case ets:lookup(base_mgw, ID) of
                [Rec | _] when is_record(Rec, base_mgw_rec) ->
                    ok;
                _ ->
                    error
            end
    end.

%%----------------------------------------------------------------------
%% Invoked when a connection is teared down
%%----------------------------------------------------------------------

handle_disconnect(ConnHandle, ProtocolVersion, Reason) ->
    log:log(debug, "handle_disconnect -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "~n   Reason:          ~p"
    "~n", [ConnHandle, ProtocolVersion, Reason]),
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
    "~n", [ReceiveHandle, ProtocolVersion, ErrorDescriptor]),
    reply.

%%----------------------------------------------------------------------
%% Invoked when a received message contained no transactions
%%----------------------------------------------------------------------

handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor) ->
    log:log(debug, "handle_message_error -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "~n   ErrorDescriptor: ~p"
    "~n", [ConnHandle, ProtocolVersion, ErrorDescriptor]),
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
    io:format("selfpid ~p~n", [self()]),
    case connHandle_in_MgwId(ConnHandle) of
        unknown ->
            ignore_trans_request;
        ID ->
            Ask = case ets:lookup(base_mgw, ID) of
                      unknown ->
                          ignore_trans_request;
                      _MgwId ->
                          ets:insert(base_request, #base_request_rec{id = self(), id_mgw = ID}),
                          mgw:work_actions(ActionRequests)
                  end,
            if Ask /= unknown ->
                ets:delete(base_request, self());
                true -> []
            end,
            Ask
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
    "~n", [ConnHandle, ProtocolVersion, ActualReply, ReplyData]),
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
    "~n", [ConnHandle, ProtocolVersion, AckStatus, AckData]),
    MgwID = connHandle_in_MgwId(ConnHandle),
    if
        (AckStatus == ok) andalso (AckData == AckData) ->
            erlang:spawn(mgw, connection_established, [MgwID]);
        true ->
            []
    end,
    ok.

%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans) ->
    log:log(debug, "handle_unexpected_trans -> entry with"
    "~n   ConnHandle:      ~p"
    "~n   ProtocolVersion: ~p"
    "~n   Trans:           ~p"
    "~n", [ConnHandle, ProtocolVersion, Trans]),
    ok.


%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_trans_request_abort(_ConnHandle, _ProtocolVersion, _TransId, _Pid) ->
    ok.

%%----------------------------------------------------------------------
%% Private function
%%----------------------------------------------------------------------

connHandle_in_MgwId(#megaco_conn_handle{
    remote_mid = {ip4Address, #'IP4Address'{address = [A, B, C, D], portNumber = Port}}
}) ->
    list_to_atom(
        integer_to_list(A) ++ "." ++
            integer_to_list(B) ++ "." ++
            integer_to_list(C) ++ "." ++
            integer_to_list(D) ++ ":" ++
            integer_to_list(Port)
    );
connHandle_in_MgwId(_) ->
    unknown.
