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
	registered_subscribers/2,
	update_subscriber_SigEv/5,
	update_subscriber_signal/4,
	update_subscriber_events/4
]).

-spec add(
	TermID :: string(),
	Number :: string(),
	MGW_Id :: atom()) -> boolean().
add(TermID, Number, MGW_Id) ->
	S1 = ets:lookup(?TABLE_SUBSCRIBER, Number),
	S2 = ets:lookup(MGW_Id, TermID),
	if
		(S1 == []) andalso (S2 == []) ->
			NewSubscriber = #base_subscriber{number = Number, mgw_id = MGW_Id, tid = TermID},
			NewLine = #base_line_rec{tid = TermID, regScript = "./lua/test.lua"},
			ets:insert_new(?TABLE_SUBSCRIBER, NewSubscriber),
			ets:insert_new(MGW_Id, NewLine),
			ok;
		S1 /= [] ->
			{error, "Number is duplicate."};
		S2 /= [] ->
			{error, "Termination in MGW is duplicate."};
		true ->
			{error, "unknown error."}
	end.

-spec registered_subscribers(
	MGW_Id :: atom(),
	ConnHandle :: term()) -> ok.
registered_subscribers(MGW_Id, ConnHandle) ->
%%	SubtractAll = message:greate_subtract(?megaco_all),
%%	ActionRequestsSub = message:greate_ActionRequest(?megaco_all_context_id, [SubtractAll]),
%%	R = megaco:call(ConnHandle, [ActionRequestsSub], []),
%%	case R of
%%		{1, {ok, _}} ->
%%			access_lua:work_registred(ets:first(MGW_Id), ConnHandle, MGW_Id),
%%			mgw:update_last_access(MGW_Id);
%%		_ ->
%%			mgw:connection_lost(MGW_Id)
%%	end,
	access_lua:work_registred(ets:first(MGW_Id), ConnHandle, MGW_Id),
	ok.

%%----------------------------------------------------------------------
%% Private function
%%----------------------------------------------------------------------
work_registred('$end_of_table', _ConnHandle, _MGW_Id) -> ok;
work_registred(Key, ConnHandle, MGW_Id) ->
%%	send ServiceChangeRequest
	[Record | _] = ets:lookup(MGW_Id, Key),
	ServiceChangeCmd = message:greate_ServiceChange(Record#base_line_rec.tid, restart, ?megaco_service_restored),
	ActionRequests = message:greate_ActionRequest(?megaco_null_context_id, [ServiceChangeCmd]),
	R = megaco:call(ConnHandle, [ActionRequests], [{request_timer, ?TIMER_MEGACO_ASK}]),
	case R of
		{1, {ok, _Ans}} ->
%%			TODO check answer (context, term_id)
			NewRec = Record#base_line_rec{service = true},
			update_subscriber_SigEv(NewRec, ConnHandle, nulltone, 3, MGW_Id),
			work_registred(ets:next(MGW_Id, Key), ConnHandle, MGW_Id);
		_ ->
			NewRec = Record#base_line_rec{service = false},
			ets:insert(MGW_Id, NewRec),
			work_registred(ets:next(MGW_Id, Key), ConnHandle, MGW_Id)
	end.

update_subscriber_signal(Record, ConnHandle, Tone, MGW_Id) ->
	SignalDescriptor = case ets:lookup(?TABLE_SIGNALS, Tone) of
		                   [Signal] when is_record(Signal, base_signals_rec) ->
			                   {signalsDescriptor, Signal#base_signals_rec.signal};
		                   _ -> []
	                   end,
	if SignalDescriptor /= [] ->
		Cmd = message:greate_AmmRequest(Record#base_line_rec.tid, [SignalDescriptor]),
		ActionRequests = message:greate_ActionRequest(
			Record#base_line_rec.context,
			[#'CommandRequest'{command = {modReq, Cmd}}]
		),
		R = megaco:call(ConnHandle, [ActionRequests], [{request_timer, ?TIMER_MEGACO_ASK}]),
		case R of
			{1, {ok, _Ans}} ->
%%  			TODO check answer (context, term_id)
				NewRec = Record#base_line_rec{signalID = Tone},
				ets:insert(MGW_Id, NewRec),
				true;
			_ ->
				false
		end;
		true -> false
	end.

update_subscriber_events(Record, ConnHandle, Event, MGW_Id) ->
	EventsDescriptor = case ets:lookup(?TABLE_EVENTS, Event) of
		                   [Events] when is_record(Events, base_events_rec) ->
			                   {eventsDescriptor, #'EventsDescriptor'{
				                   requestID = Events#base_events_rec.id,
				                   eventList = Events#base_events_rec.events
			                   }};
		                   _ -> []
	                   end,
	if EventsDescriptor /= [] ->
		Cmd = message:greate_AmmRequest(Record#base_line_rec.tid, [EventsDescriptor]),
		ActionRequests = message:greate_ActionRequest(
			Record#base_line_rec.context,
			[#'CommandRequest'{command = {modReq, Cmd}}]
		),
		R = megaco:call(ConnHandle, [ActionRequests], [{request_timer, ?TIMER_MEGACO_ASK}]),
		case R of
			{1, {ok, _Ans}} ->
%%  			TODO check answer (context, term_id)
				NewRec = Record#base_line_rec{eventID = Event},
				ets:insert(MGW_Id, NewRec),
				true;
			_ ->
				false
		end;
		true -> false
	end.

update_subscriber_SigEv(Record, ConnHandle, Tone, Event, MGW_Id) when Record#base_line_rec.service ->
%%	set signal and events
	EventsDescriptor = case ets:lookup(?TABLE_EVENTS, Event) of
		                   [Events] when is_record(Events, base_events_rec) ->
			                   {eventsDescriptor, #'EventsDescriptor'{
				                   requestID = Events#base_events_rec.id,
				                   eventList = Events#base_events_rec.events
			                   }};
		                   _ -> []
	                   end,
	SignalDescriptor = case ets:lookup(?TABLE_SIGNALS, Tone) of
		                   [Signal] when is_record(Signal, base_signals_rec) ->
			                   {signalsDescriptor, Signal#base_signals_rec.signal};
		                   _ -> []
	                   end,
	if (EventsDescriptor /= []) andalso (SignalDescriptor /= []) ->
		Cmd = message:greate_AmmRequest(Record#base_line_rec.tid, [EventsDescriptor, SignalDescriptor]),
		ActionRequests = message:greate_ActionRequest(
			Record#base_line_rec.context,
			[#'CommandRequest'{command = {modReq, Cmd}}]
		),
		R = megaco:call(ConnHandle, [ActionRequests], [{request_timer, ?TIMER_MEGACO_ASK}]),
		case R of
			{1, {ok, _Ans}} ->
%%  			TODO check answer (context, term_id)
				NewRec = Record#base_line_rec{eventID = Event, signalID = Tone},
				ets:insert(MGW_Id, NewRec),
				true;
			_ ->
				false
		end;
		true -> false
	end;

update_subscriber_SigEv(_Record, _ConnHandle, _Tone, _Event, _MGW_Id) ->
	false.
