%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Нояб. 2017 13:42
%%%-------------------------------------------------------------------
-module(message).
-author("rus").

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
%% API
-export([
	greate_ActionRequest/2,
	greate_ServiceChange/3,
	greate_subtract/1,
	greate_AmmRequest/2,


	greate_CommandAdd/1,
	greate_CommandModify/1,
	greate_CommandMove/1,
	greate_StreamParms/3,
	greate_MediaDescriptor/1
]).

timeISO8610() ->
	{{Year, Month, Day}, {Hour, Min, Sec}} =
		calendar:universal_time(),
	D = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w",
		[Year, Month, Day])),
	T = lists:flatten(io_lib:format("~2..0w~2..0w~4..0w",
		[Hour, Min, Sec])),
	#'TimeNotation'{date = D, time = T}.

greate_ActionRequest(Ctx, Comands) ->
	#'ActionRequest'{contextId = Ctx, commandRequests = Comands}.

greate_ServiceChange(TermID, Method, Reason) ->
	SCP = #'ServiceChangeParm'{serviceChangeMethod = Method,
		serviceChangeReason = [Reason],
		timeStamp = timeISO8610()},
	SCR = #'ServiceChangeRequest'{terminationID = [#megaco_term_id{id = [TermID]}],
		serviceChangeParms = SCP},
	#'CommandRequest'{command = {serviceChangeReq, SCR}}.

greate_subtract(TermID) when TermID == ?megaco_all ->
	S = #'SubtractRequest'{terminationID = [#megaco_term_id{id = [[$*]], contains_wildcards = true}]},
	#'CommandRequest'{command = {subtractReq, S}};
greate_subtract(TermID)->
	S = #'SubtractRequest'{terminationID = [#megaco_term_id{id = [TermID]}]},
	#'CommandRequest'{command = {subtractReq, S}}.

greate_AmmRequest(TermID, ListDescriptor) when TermID == ?megaco_all ->
	#'AmmRequest'{
		terminationID = [#megaco_term_id{id = [#megaco_term_id{id = [[$*]], contains_wildcards = true}]}],
		descriptors = ListDescriptor
	};
greate_AmmRequest(TermID, ListDescriptor) ->
	#'AmmRequest'{
		terminationID = [#megaco_term_id{id = [TermID]}],
		descriptors = ListDescriptor
	}.

greate_StreamParms(LCD, LD, RD) ->
	{oneStream, #'StreamParms'{
		localControlDescriptor = LCD,
		localDescriptor = LD,
		remoteDescriptor = RD
	}}.

greate_MediaDescriptor(StreamParms) ->
	{mediaDescriptor, #'MediaDescriptor'{streams = StreamParms}}.

greate_CommandAdd(AmmRequest) ->
	#'CommandRequest'{command = {addReq, AmmRequest}}.
greate_CommandModify(AmmRequest) ->
	#'CommandRequest'{command = {modReq, AmmRequest}}.
greate_CommandMove(AmmRequest) ->
	#'CommandRequest'{command = {moveReq, AmmRequest}}.
