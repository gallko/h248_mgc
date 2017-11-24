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
	callback_events/4


%%	update_subscriber_SigEv/5,
%%	update_subscriber_signal/4,
%%	update_subscriber_events/4
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
			NewLine = #base_line_rec{tid = TermID, regScript = "./lua/test.lua", table = MGW_Id},
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
	access_lua:work_registred(ets:first(MGW_Id), ConnHandle, MGW_Id),
	ok.

callback_events(ConnHandle, Ctx, RecTid, #'ObservedEventsDescriptor'{
	requestId = IdEvent, observedEventLst = [ObservedEvent | _]})
	when IdEvent == RecTid#base_line_rec.eventID ->
%%	TODO проверить ID events и сам эвент
%%	'ObservedEventsDescriptor'
	case check_events(RecTid, ObservedEvent) of
		error ->
			[];
		Event ->
			try process_info(RecTid#base_line_rec.pid_awaiting) of
				undefined when Event == off ->
					access_lua:start_talk(ConnHandle, Ctx, RecTid);
				InfoPid when is_list(InfoPid) andalso is_atom(Event) ->
%%			        TODO кто-то ждет
					RecTid#base_line_rec.pid_awaiting!Event;
				_ -> []
			catch
				_:_ when Event == 'of' ->
					access_lua:start_talk(ConnHandle, Ctx, RecTid);
				_:_ ->
					[]
			end
	end;

callback_events(_T1, _T2, _T3, _T4) ->
	[].


%%----------------------------------------------------------------------
%% Private function
%%----------------------------------------------------------------------

check_events(_RecTid, #'ObservedEvent'{eventName = Event}) when is_list(Event) ->
	case string:split(Event, [$/]) of
		["al", Id] ->
			list_to_atom(Id); %% off/on
		[_Packet, _Id] ->
			uncknown;
		_ ->
			error
	end;
check_events(_RecTid, _ObservedEvent) ->
	[].