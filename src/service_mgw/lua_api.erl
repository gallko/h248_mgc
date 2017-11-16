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
%% sendModify          = 4


erlCallbackFunc([Command | Params], S) when is_number(Command) ->
	Cmd = trunc(Command),
	[ConnHandle_B | Prms] = Params,
	try binary_to_term(ConnHandle_B) of
		T when is_record(T, megaco_conn_handle) ->
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
erlCallbackFunc(1, ConnHandle, [TermID_B], S) when is_bitstring(TermID_B) ->
	TermID = string:to_upper(binary_to_list(TermID_B)),
	ServiceChangeCmd = message:greate_ServiceChange(TermID, restart, ?megaco_service_restored),
	ActionRequests = message:greate_ActionRequest(?megaco_null_context_id, [ServiceChangeCmd]),
	R = megaco:call(ConnHandle, [ActionRequests], [{request_timer, ?TIMER_MEGACO_ASK}]),
	case R of
		{1, {ok, _Ans}} ->
%%			TODO check answer (context, term_id)
			{[0], S};
		_ ->
			{[2], S}
	end;

%% subtractAll
erlCallbackFunc(2, ConnHandle, [TermID_B], S) when is_bitstring(TermID_B) ->
	TermID = string:to_upper(binary_to_list(TermID_B)),
	SubtractAll = message:greate_subtract(TermID),
	ActionRequestsSub = message:greate_ActionRequest(?megaco_all_context_id, [SubtractAll]),
	R = megaco:call(ConnHandle, [ActionRequestsSub], []),
	case R of
		{1, {ok, _Ans}} ->
%%			TODO check answer (context, term_id)
			{[0], S};
		_ ->
			{[2], S}
	end;

%% subtract
erlCallbackFunc(3, ConnHandle, [Ctx_D, TermID_B], S) when is_number(Ctx_D) andalso is_bitstring(TermID_B) ->
	Context = trunc(Ctx_D),
	TermID = string:to_upper(binary_to_list(TermID_B)),
	SubtractAll = message:greate_subtract(TermID),
	ActionRequestsSub = message:greate_ActionRequest(Context, [SubtractAll]),
	R = megaco:call(ConnHandle, [ActionRequestsSub], []),
	case R of
		{1, {ok, _Ans}} ->
%%			TODO check answer (context, term_id)
			{[0], S};
		_ ->
			{[2], S}
	end;

%% sendModify
erlCallbackFunc(4, _ConnHandle, Param, S) ->
%%	Param = [Ctx, TermID, Events, Signal, StreamMode, ReserveValue, ReserveGroup, tdmc_EchoCancel, tdmc_Gain]
	io:format("~p~n", [Param]),
	{[0], S};

erlCallbackFunc(_Cmd, _ConnHandle, _List, S) ->
	{[1], S}.

%%----------------------------------------------------------------------
%% HELP function
%%----------------------------------------------------------------------

parse_param4(Params, I) ->
%%	[nil,<<"a9">>,nil,nil,nil,nil,nil,nil,nil]
	parse_param4(Params, 0, []).
parse_param4(_Params, 9, Result) -> Result;
parse_param4([_H | T], I, Result) ->
	parse_param4(T, I - 1, Result).