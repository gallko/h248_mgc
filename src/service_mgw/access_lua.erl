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

%%
-export([
	run2/0
]).
%% API
-export([
	work_registred/3
]).

run2() ->
%%	LuaScript = <<"hello_table = { hello=\"world\" }; return hello_table">>,
%%	{[_Table], Lua0} = luerl:do(LuaScript),
%%
%%	{World, Lua1} = luerl:get_table([hello_table, hello], Lua0),
%%
%%
%%	Lua2 = luerl:set_table([connHandle], fg, Lua0),
%%	{HelloDict,Lua3} = luerl:get_table([connHandle], Lua2),
%%
%%	HelloDict1 = luerl:decode(HelloDict, Lua3),


[].

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
%%				Record#base_line_rec.context,
%%				list_to_binary(Record#base_line_rec.signalID),
%%				Record#base_line_rec.eventID,
				term_to_binary(ConnHandle)
			], State2)
	catch
		_:_ ->
			io:format("Opps..~n")
	end,
	luerl:stop(State2),
	work_registred(ets:next(MGW_Id, Key), ConnHandle, MGW_Id).

%%----------------------------------------------------------------------
%% Private function
%%----------------------------------------------------------------------

load_func_registred(State) ->
	luerl:set_table([<<"erlCallbackFunc">>], fun lua_api:erlCallbackFunc/2, State).

load_lua_file(File, State) ->
	{_, S} = luerl:dofile(File, State),
	S.