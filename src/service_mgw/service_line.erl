%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Нояб. 2017 9:31
%%%-------------------------------------------------------------------
-module(service_line).
-author("rus").

-include_lib("stdlib/include/ms_transform.hrl").

-include("../include/struct_load.hrl").
-include("../include/define_mgc.hrl").

%% API
-export([
	add/3,
	registered_subscribers/2
]).

-spec add(
	TermID :: string(),
	Number :: string(),
	MGW_Id :: atom()) -> ok | error.
add(TermID, Number, MGW_Id) ->
	ID = list_to_atom(atom_to_list(MGW_Id) ++ "-" ++ string:to_upper(TermID)),
	NewLine = #base_line_rec{id = ID, id_mgw = MGW_Id, number = Number, tid = TermID},
	ets:insert_new(?TABLE_SUBSCRIBER, NewLine).

-spec registered_subscribers(
	MGW_Id :: atom(),
	ConnHandle :: term()) -> ok.
registered_subscribers(MGW_Id, ConnHandle) ->
	SubtractAll = message:greate_subtract(?megaco_all),
	ActionRequestsSub = message:greate_ActionRequest(?megaco_all_context_id, [SubtractAll]),
	R = megaco:call(ConnHandle, [ActionRequestsSub], []),
	MS = ets:fun2ms(
		fun(Record) when Record#base_line_rec.id_mgw == MGW_Id -> Record end
	),
	case R of
		{1, {ok, _}} ->
			work_registred(ets:select(?TABLE_SUBSCRIBER, MS), ConnHandle),
			mgw:update_last_access(MGW_Id);
		_ ->
			mgw:connection_lost(MGW_Id)
	end,
	ok.

%%----------------------------------------------------------------------
%% Private function
%%----------------------------------------------------------------------
work_registred([], _ConnHandle) -> ok;
work_registred([Record | Records], ConnHandle) ->
%%	send ServiceChangeRequest
	ServiceChangeCmd = message:greate_ServiceChange(Record#base_line_rec.tid, restart, ?megaco_service_restored),
	ActionRequests = message:greate_ActionRequest(?megaco_null_context_id, [ServiceChangeCmd]),
	R = megaco:call(ConnHandle, [ActionRequests], [{request_timer, ?TIMER_MEGACO_ASK}]),
	case R of
		{1, {ok, _}} ->
			update_subscriber(Record, ConnHandle),
			work_registred(Records, ConnHandle);
		_ ->
			work_registred(Records, ConnHandle)
	end.

update_subscriber(Record, ConnHandle) ->
%%	set signal and events
%%	megaco:deco

%%	TODO get base_mgw_events:id
	EventALOF = #'RequestedEvent'{pkgdName = "al/of"},
	EventsDescriptor = {eventsDescriptor, #'EventsDescriptor'{requestID = 1, eventList = [EventALOF]}},

%%	TODO get base_mgw_signal:id
	SignalDescriptor = {signalsDescriptor, []},
	
	Cmd = message:greate_AmmRequest(Record#base_line_rec.tid, [EventsDescriptor, SignalDescriptor]),
	ActionRequests = message:greate_ActionRequest(?megaco_null_context_id, [#'CommandRequest'{command = {modReq, Cmd}}]),
	R = megaco:call(ConnHandle, [ActionRequests], [{request_timer, ?TIMER_MEGACO_ASK}]),
	io:format("Event ~p~n", [R]).