%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Нояб. 2017 15:09
%%%-------------------------------------------------------------------
-module(access_lua).
-author("rus").
-include("../include/struct_load.hrl").
-include("../include/define_mgc.hrl").

%% API
-export([
	work_registred/3,
	start_talk/3
]).

work_registred('$end_of_table', _ConnHandle, _MGW_Id) -> ok;
work_registred(Key, ConnHandle, MGW_Id) ->
	State = luerl:init(),
	State1 = load_func_registred(State),
	[Record | _] = ets:lookup(MGW_Id, Key),
	State2 = load_lua_file(Record#base_line_rec.regScript, State1),
	try
		{_Res, _State3} = luerl:call_function([registration],
			[
				list_to_binary(Record#base_line_rec.tid),
				Record#base_line_rec.context,
%%				list_to_binary(Record#base_line_rec.signalID),
%%				Record#base_line_rec.eventID,
				term_to_binary(#info_lua{conn_handle = ConnHandle, record_tid = Record})
			], State2)
	catch
		_:_ ->
			io:format("Opps..~n")
	end,
	luerl:stop(State2),
	work_registred(ets:next(MGW_Id, Key), ConnHandle, MGW_Id).

start_talk(ConnHandle, Ctx, RecTid) ->
	State = luerl:init(),
	State1 = load_func_registred(State),
	State2 = load_lua_file(RecTid#base_line_rec.regScript, State1),
	try
		{_Res, _State3} = luerl:call_function([start_talk],
			[
				list_to_binary(RecTid#base_line_rec.tid),
				Ctx,
%%				list_to_binary(Record#base_line_rec.signalID),
%%				Record#base_line_rec.eventID,
				term_to_binary(#info_lua{conn_handle = ConnHandle, record_tid = RecTid})
			], State2)
	catch
		_:_ ->
			io:format("Opps..~n")
	end,
	luerl:stop(State2).

%%----------------------------------------------------------------------
%% Private function
%%----------------------------------------------------------------------

load_func_registred(State) ->
	luerl:set_table([<<"erlCallbackFunc">>], fun lua_api:erlCallbackFunc/2, State).

load_lua_file(File, State) ->
	{_, S} = luerl:dofile(File, State),
	S.