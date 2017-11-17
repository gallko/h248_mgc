%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Нояб. 2017 9:31
%%%-------------------------------------------------------------------
-module(lua_api).
-author("rus").

-include("../include/struct_load.hrl").
-include("../include/define_mgc.hrl").

%% API
-export([
	erlCallbackFunc/2
]).

%% TEST
-export([
	parse_param4/2,
	test/0
]).

%%----------------------------------------------------------------------
%% API LUA function
%%----------------------------------------------------------------------

%% error
%% [0]     = 'Successes'
%% [1]     = 'Bad arguments'
%% [2]     = 'Timeout of replay'
%% [100]   = 'Binary data is wrong'

%% function
%% sendRestoreService  = 1+
%% subtractAll         = 2+
%% subtract            = 3+
%% sendModifyRTID      = 4+


erlCallbackFunc([Command | Params], S) when is_number(Command) ->
	Cmd = trunc(Command),
	[ConnHandle_B | Prms] = Params,
	try binary_to_term(ConnHandle_B) of
		T when is_record(T, info_lua) ->
			erlCallbackFunc(Cmd, T, Prms, S);
		_ ->
			{[100], S}
	catch
		_:_ ->
			{[100], S}
	end;

erlCallbackFunc(_List, S) ->
	{[1], S}.

%% sendRestoreServPack
erlCallbackFunc(1, InfoData, [TermID_B], S) when is_bitstring(TermID_B) ->
	TermID = string:to_lower(binary_to_list(TermID_B)),
	ServiceChangeCmd = message:greate_ServiceChange(TermID, restart, ?megaco_service_restored),
	ActionRequests = message:greate_ActionRequest(?megaco_null_context_id, [ServiceChangeCmd]),
	R = megaco:call(InfoData#info_lua.conn_handle, [ActionRequests], [{request_timer, ?TIMER_MEGACO_ASK}]),
	case R of
		{1, {ok, _Ans}} ->
%%			TODO check answer (context, term_id)
			{[0], S};
		_ ->
			{[2], S}
	end;

%% subtractAll
erlCallbackFunc(2, InfoData, [TermID_B], S) when is_bitstring(TermID_B) ->
	TermID = string:to_lower(binary_to_list(TermID_B)),
	SubtractAll = message:greate_subtract(TermID),
	ActionRequestsSub = message:greate_ActionRequest(?megaco_all_context_id, [SubtractAll]),
	R = megaco:call(InfoData#info_lua.conn_handle, [ActionRequestsSub], [{request_timer, ?TIMER_MEGACO_ASK}]),
	case R of
		{1, {ok, _Ans}} ->
%%			TODO check answer (context, term_id)
			{[0], S};
		_ ->
			{[2], S}
	end;

%% subtract
erlCallbackFunc(3, InfoData, [Ctx_D, TermID_B], S) when is_number(Ctx_D) andalso is_bitstring(TermID_B) ->
	Context = trunc(Ctx_D),
	TermID = string:to_lower(binary_to_list(TermID_B)),
	SubtractAll = message:greate_subtract(TermID),
	ActionRequestsSub = message:greate_ActionRequest(Context, [SubtractAll]),
	R = megaco:call(InfoData#info_lua.conn_handle, [ActionRequestsSub], [{request_timer, ?TIMER_MEGACO_ASK}]),
	case R of
		{1, {ok, _Ans}} ->
%%			TODO check answer (context, term_id)
			{[0], S};
		_ ->
			{[2], S}
	end;

%% sendModify
erlCallbackFunc(4, InfoData, Param, S) ->
%%	Param = [Ctx, TermID, Events, Signal, StreamMode, ReserveValue, ReserveGroup, tdmc_EchoCancel, tdmc_Gain]
	case parse_param4(Param, InfoData#info_lua.record_tid) of
		{error, Description} ->
			{[1], S};
		MapPrm ->
			MediaDescriptor = case maps:is_key('LocalControlDescriptor', MapPrm) of
				                  true ->
					                  LCD = maps:get('LocalControlDescriptor', MapPrm),
					                  StreamParms = message:greate_StreamParms(LCD, asn1_NOVALUE, asn1_NOVALUE),
					                  message:greate_MediaDescriptor(StreamParms);
				                  false -> asn1_NOVALUE
			                  end,
			EventsDescriptor = case maps:is_key('RequestedEvent', MapPrm) of
				                   true ->
					                   maps:get('RequestedEvent', MapPrm);
				                   false -> asn1_NOVALUE
			                   end,
			SignalDescriptor = case maps:is_key('Signal', MapPrm) of
				                   true ->
					                   maps:get('Signal', MapPrm);
				                   false -> asn1_NOVALUE
			                   end,
			AmmRequest = message:greate_AmmRequest(maps:get(termID, MapPrm),
				[MediaDescriptor, EventsDescriptor, SignalDescriptor]),
			CommandRequest = message:greate_CommandModify(AmmRequest),
			ActionRequest = message:greate_ActionRequest(maps:get(context, MapPrm), [CommandRequest]),
			R = megaco:call(InfoData#info_lua.conn_handle, [ActionRequest], [{request_timer, ?TIMER_MEGACO_ASK}]),
			case R of
				{1, {ok, _Ans}} ->
%%			TODO check answer (context, term_id)
					{[0], S};
				_ ->
					{[2], S}
			end
	end;

erlCallbackFunc(_Cmd, _ConnHandle, _List, S) ->
	{[1], S}.

%%----------------------------------------------------------------------
%% HELP function
%%----------------------------------------------------------------------

parse_param4(Params, RecordTid) ->
%%	[nil, <<"a9">>, nil,    nil,    nil,        nil,          nil,          nil,             nil]
%%	[Ctx, TermID,   Events, Signal, StreamMode, ReserveValue, ReserveGroup, tdmc_EchoCancel, tdmc_Gain]
%%	[int, str,      int,    str
	parse_param4(Params, 0, maps:new(), RecordTid).
parse_param4(_Params, 9, Result, _RecordTid) -> Result;
parse_param4([H | T], I, Map, RecordTid) ->
	{K, V} = case I of
		         0 when is_number(H) -> {context, trunc(H)};
		         0 when H == nil ->     {context, 0};

		         1 ->
			         try string:to_lower(binary_to_list(H)) of
						 TermId when TermId == RecordTid#base_line_rec.tid -> {termID, TermId};
						 _ -> {error, "Can't parse parametrs TermID"}
			         catch
			            _:_ -> {error, "Can't parse parametrs"}
			         end;
		         2 when H == nil -> {no_use, nil};
		         2 when is_number(H) ->
			         try ets:lookup(?TABLE_EVENTS, trunc(H)) of
						 [Event | _] ->
							 {'RequestedEvent', {eventsDescriptor, #'EventsDescriptor'{
								 requestID = Event#base_events_rec.id,
								 eventList = Event#base_events_rec.events
							 }}};
						 [] -> {error, "Can't not find event ID"}
			         catch
				         _:_ -> {error, "Don't find table of events"}
			         end;

		         3 when H == nil -> {no_use, nil};
				 3 ->
			         try ets:lookup(?TABLE_SIGNALS, binary_to_list(H)) of
				         [Signal | _] ->
					         {'Signal', {signalsDescriptor, Signal#base_signals_rec.signal}};
				         [] -> {error, "Can't parse parametrs Signal"}
			         catch
				         _:_ -> {error, "Don't find table of signal"}
			         end;

				 4 when H == nil -> {no_use, nil};
		         4 ->
			         try binary_to_atom(H, latin1) of
				         StreamMode when (StreamMode =:= sendOnly)
					         orelse (StreamMode =:= recvOnly )orelse (StreamMode =:= inactive)
					         orelse (StreamMode =:= sendRecv) orelse (StreamMode =:= loopBack) ->
					         case maps:is_key('LocalControlDescriptor', Map) of
					             true ->
					                StreamParms = maps:get('LocalControlDescriptor', Map),
							        {'LocalControlDescriptor',
								        StreamParms#'LocalControlDescriptor'{streamMode = StreamMode}};
								 false ->
									 {'LocalControlDescriptor',
										 #'LocalControlDescriptor'{streamMode = StreamMode}}
					         end;
				         _ -> {error, "Can't parse parametrs StreamMode"}
			         catch
				         _:_ -> {error, "Can't parse parametrs"}
			         end;

		         5 when is_boolean(H) ->
			         case maps:is_key('LocalControlDescriptor', Map) of
				         true ->
					         StreamParms = maps:get('LocalControlDescriptor', Map),
					         {'LocalControlDescriptor',
						         StreamParms#'LocalControlDescriptor'{reserveValue = H}};
				         false ->
					         {'LocalControlDescriptor',
						         #'LocalControlDescriptor'{reserveValue = H}}
			         end;
				 5 when H == nil -> {no_use, H};
				 5 -> {error, "Can't parse parametrs ReserveValue"};

		         6 when is_boolean(H) ->
			         case maps:is_key('LocalControlDescriptor', Map) of
				         true ->
					         StreamParms = maps:get('LocalControlDescriptor', Map),
					         {'LocalControlDescriptor',
						         StreamParms#'LocalControlDescriptor'{reserveGroup = H}};
				         false ->
					         {'LocalControlDescriptor',
						         #'LocalControlDescriptor'{reserveGroup = H}}
			         end;
		         6 when H == nil -> {no_use, H};
		         6 -> {error, "Can't parse parametrs ReserveGroup"};

%%				 [#'PropertyParm']
		         7 when is_boolean(H) ->
			         Value = case H of
				                 true -> ["on"];
				                 false -> ["off"]
			                 end,
			         PropertyParms = #'PropertyParm'{name = "tdmc/ec", value = Value},
			         case maps:is_key('LocalControlDescriptor', Map) of
				         true ->
					         StreamParms = maps:get('LocalControlDescriptor', Map),
					         NewPropertyParms = StreamParms#'LocalControlDescriptor'.propertyParms ++ [PropertyParms],
					         {'LocalControlDescriptor',
						         StreamParms#'LocalControlDescriptor'{propertyParms = NewPropertyParms}};
				         false ->
					         {'LocalControlDescriptor',
						         #'LocalControlDescriptor'{propertyParms = [PropertyParms]}}
			         end;
		         7 when H == nil -> {no_use, H};
				 7 -> {error, "Can't parse parametrs tdmc_EchoCancel"};

		         8 when is_number(H) ->
			         PropertyParms = #'PropertyParm'{name = "tdmc/gain", value = [integer_to_list(trunc(H))]},
			         case maps:is_key('LocalControlDescriptor', Map) of
				         true ->
					         StreamParms = maps:get('LocalControlDescriptor', Map),
					         NewPropertyParms = StreamParms#'LocalControlDescriptor'.propertyParms ++ [PropertyParms],
					         {'LocalControlDescriptor',
						         StreamParms#'LocalControlDescriptor'{propertyParms = NewPropertyParms}};
				         false ->
					         {'LocalControlDescriptor',
						         #'LocalControlDescriptor'{propertyParms = [PropertyParms]}}
			         end;
		         8 when H == nil -> {no_use, H};
				 8 -> {error, "Can't parse parametrs tdmc_Gain"};

				 _ -> {error, "Can't parse parametrs"}
	         end,
	case K of
		error ->
			{K, V};
		no_use ->
			parse_param4(T, I + 1, Map, RecordTid);
		_ ->
			NewMap = maps:put(K, V, Map),
			parse_param4(T, I + 1, NewMap, RecordTid)
	end.


%%----------------------------------------------------------------------
%% TEST function
%%----------------------------------------------------------------------
test() ->
	ets:new(?TABLE_SIGNALS, [ordered_set, public, named_table, {keypos, #base_signals_rec.id}]),
	ets:new(?TABLE_EVENTS, [ordered_set, public, named_table, {keypos, #base_signals_rec.id}]),

	ets:insert_new(?TABLE_SIGNALS, #base_signals_rec{
		id = "null",
		signal = []
	}),
	ets:insert_new(?TABLE_SIGNALS, #base_signals_rec{
		id = "cg/dt",
		signal = [{signal, #'Signal'{signalName = "cg/dt"}}]
	}),
	ets:insert_new(?TABLE_SIGNALS, #base_signals_rec{
		id = "cg/rt",
		signal = [{signal, #'Signal'{signalName = "cg/rt"}}]
	}),
	ets:insert_new(?TABLE_SIGNALS, #base_signals_rec{
		id = "cg/bt",
		signal = [{signal, #'Signal'{signalName = "cg/bt"}}]
	}),
	ets:insert_new(?TABLE_SIGNALS, #base_signals_rec{
		id = "al/ri",
		signal = [{signal, #'Signal'{signalName = "al/ri"}}]
	}),
	ets:insert_new(?TABLE_EVENTS, #base_events_rec{
		id = 1,
		events = [#'RequestedEvent'{pkgdName = "al/of"}]
	}),
	ets:insert_new(?TABLE_EVENTS, #base_events_rec{
		id = 2,
		events = [#'RequestedEvent'{pkgdName = "al/on"}]
	}),
	ets:insert_new(?TABLE_EVENTS, #base_events_rec{
		id = 3,
		events = [
			#'RequestedEvent'{pkgdName = "al/of"},
			#'RequestedEvent'{pkgdName = "al/on"}
		]
	}),
%%	       [Ctx, TermID,   Events, Signal,      StreamMode,     ReserveValue, ReserveGroup, tdmc_EchoCancel, tdmc_Gain]
	List = [5,   <<"a9">>, 3,      <<"cg/dt">>, nil, nil,        nil,         nil,           nil],
	Rec = #base_line_rec{tid = "a9", regScript = ""},
	Rs = parse_param4(List, Rec),
	case Rs of
		{error, Description} ->
			{[1]};
		MapPrm ->
			MediaDescriptor = case maps:is_key('LocalControlDescriptor', MapPrm) of
				                  true ->
					                  LCD = maps:get('LocalControlDescriptor', MapPrm),
					                  StreamParms = message:greate_StreamParms(LCD, asn1_NOVALUE, asn1_NOVALUE),
					                  message:greate_MediaDescriptor(StreamParms);
				                  false -> asn1_NOVALUE
			                  end,
			EventsDescriptor = case maps:is_key('RequestedEvent', MapPrm) of
				                   true ->
					                   maps:get('RequestedEvent', MapPrm);
				                   false -> asn1_NOVALUE
			                   end,
			SignalDescriptor = case maps:is_key('RequestedEvent', MapPrm) of
				                   true ->
					                   maps:get('Signal', MapPrm);
				                   false -> asn1_NOVALUE
			                   end,
			AmmRequest = message:greate_AmmRequest(maps:get(termID, MapPrm),
				[MediaDescriptor, EventsDescriptor, SignalDescriptor]),
			CommandRequest = message:greate_CommandModify(AmmRequest),
			ActionRequest = message:greate_ActionRequest(maps:get(context, MapPrm), [CommandRequest]),
			ActionRequest
	end.