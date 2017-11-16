%%%-------------------------------------------------------------------
%% @doc h248_mgc public API
%% @end
%%%-------------------------------------------------------------------

-module(h248_mgc_app).

-behaviour(application).

-include("../include/struct_load.hrl").
-include("../include/define_mgc.hrl").

%%-include_lib("eunit/include/eunit.hrl").

%% Public
-export([start/0]).

%% Application callbacks
-export([
	start/2,
	stop/1
]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	log:log(notify, "start application [~p]~n", [?MODULE]),
	megaco:start(),
	megaco:enable_trace(min, io),
	Return = h248_mgc_sup:start_link(),

%%    init table
	ets:new(?TABLE_MGW, [ordered_set, public, named_table, {keypos, #base_mgw_rec.id_rec}]),
	ets:new(?TABLE_REQUEST,[ordered_set, public, named_table, {keypos, #base_request_rec.id}]),
	ets:new(?TABLE_SUBSCRIBER, [ordered_set, public, named_table, {keypos, #base_subscriber.number}]),
	ets:new(?TABLE_SIGNALS, [ordered_set, public, named_table, {keypos, #base_signals_rec.id}]),
	ets:new(?TABLE_EVENTS, [ordered_set, public, named_table, {keypos, #base_signals_rec.id}]),

	ping_mgw:start(),
	start_defualt_for_debug(),

	Return.

%%--------------------------------------------------------------------
stop(_State) ->
	log:log(notify, "stop application [~p]~n", [?MODULE]),
	ok.

%%====================================================================
%% Public functions
%%====================================================================
start() ->
	application:start(h248_mgc),
	loop().


%%====================================================================
%% Debug functions
%%====================================================================

start_defualt_for_debug() ->
	Mid_MGC = mgc:add_user("192.168.0.81", 2944, callback, []),
	mgc:add_transport(Mid_MGC, megaco_pretty_text_encoder, megaco_udp),
	
%%	prepare events and signals
	fill_events_table(),
	fill_signals_table(),


%% 	prepare add MGW and subscribers
	case mgw:add(Mid_MGC, "192.168.0.143", 72) of
		{ok, MGw} when is_atom(MGw) ->
			gen_line(30, "143", MGw);
		{error, _} ->
			[]
	end,
	case mgw:add(Mid_MGC, "192.168.0.146", 36) of
		{ok, MGw1} when is_atom(MGw1) ->
			gen_line(5, "146", MGw1);
		{error, _} ->
			[]
	end.

gen_line(-1, _Suffix, _MGW) -> ok;
gen_line(Number, Suffix, MGW) ->
	service_line:add("a" ++ integer_to_list(Number), Suffix ++ integer_to_list(Number), MGW),
	gen_line(Number - 1, Suffix, MGW).


fill_signals_table() ->
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
	[].

fill_events_table() ->
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
	}).


loop() ->
	receive
		_Msg ->
			[]
	end,
	loop().
