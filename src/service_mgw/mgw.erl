%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Сент. 2017 13:46
%%%-------------------------------------------------------------------
-module(mgw).
-author("rus").

%%-include_lib("megaco/include/megaco.hrl").
%%-include_lib("megaco/include/megaco_message_v1.hrl").
-include("../include/struct_load.hrl").
-include("../include/define_mgc.hrl").

-include_lib("stdlib/include/ms_transform.hrl").


%% API
-export([
	add/3, add/4,
%%	delete/2,

	connection_lost/1,
	connection_established/1,
	update_last_access/1,

	work_actions/1
]).

%%%===================================================================
%%% Test functions
%%%===================================================================

-export([test/1]).
test(Key) ->
	MS = ets:fun2ms(fun(#base_mgw_rec{id_rec = Id, count_port = Port}) when Port == Key -> Id end),
	MS.
%%    ets:select(base_mgw, MS).
%%%===================================================================
%%% Public functions
%%%===================================================================

add(MidMGC, Ip, NumPorts) ->
	add(MidMGC, Ip, ?megaco_ip_port_text, NumPorts).
add(MidMGC, Ip, Port, NumPorts) ->
	Addr = case inet:parse_address(Ip) of
		       {ok, A} ->
			       tuple_to_list(A);
		       {error, einval} ->
			       log:log(warning, "ip not correct, set mid user 127.0.0.1~n"),
			       [127, 0, 0, 1]
	       end,
	MidMGW = {ip4Address, #'IP4Address'{address = Addr, portNumber = Port}},
	MgwName = list_to_atom(Ip ++ ":" ++ integer_to_list(Port)),
	ets:new(MgwName, [ordered_set, public, named_table, {keypos, #base_line_rec.tid}]),
	ets:insert(base_mgw, #base_mgw_rec{
		id_rec = MgwName,
		mid_mgw = MidMGW,
		mid_mgc = MidMGC,
		count_port = NumPorts,
		status = offline}),
	log:log(notify, "Add MGW [~p]... ok~n", [MgwName]),
	{ok, MgwName}.

%% return ok | []
%delete(IdMGW) ->
%	case base_mgw:get_rec(IdMGW, conn_handle) of
%		unknown ->
%			[];
%		ConnHandle when is_record(ConnHandle, megaco_conn_handle) ->
%			ets:delete(base_mgw, IdMGW),
%%%            base_mgw:delete(IdMGW),
%			case megaco:disconnect(ConnHandle, delete_adm) of
%				ok -> ok;
%				{error, _Reason} ->
%					[]
%			end;
%		_ ->
%			[]
%	end.
%delete(Ip, Port) when is_list(Ip) and is_integer(Port) ->
%	IdMGW = list_to_atom(Ip ++ ":" ++ integer_to_list(Port)),
%	delete(IdMGW);
%delete(_Ip, _Port) ->
%	[].

connection_lost(KeyMG) ->
	case ets:lookup(base_mgw, KeyMG) of
		[Rec | _] ->
			io:format("Connection lost ~p~n", [KeyMG]),
			NewRec = Rec#base_mgw_rec{status = offline, last_access = []},
			ets:insert(base_mgw, NewRec);
		[] ->
			[]
	end.

connection_established(ConnHandle) ->
	KeyMG = connHandle_in_MgwId(ConnHandle),
%%	detached thread
	case ets:lookup(base_mgw, KeyMG) of
		[Rec | _] ->
			io:format("Connection established ~p~n", [KeyMG]),
%%			TODO connection established, load subscriber
			NewRec = Rec#base_mgw_rec{status = online, last_access = erlang:timestamp()},
			ets:insert(base_mgw, NewRec),
			service_line:registered_subscribers(KeyMG, ConnHandle),
			[];
		[] ->
			[]
	end.

update_last_access(MGW) when is_atom(MGW)->
	case ets:lookup(base_mgw, MGW) of
		[Rec | _] ->
			NewRec = Rec#base_mgw_rec{last_access = erlang:timestamp()},
			ets:insert(?TABLE_MGW, NewRec);
		[] ->
			[]
	end;
update_last_access(MGW) when is_record(MGW, megaco_conn_handle)->
	KeyMG = connHandle_in_MgwId(MGW),
	update_last_access(KeyMG).

get_ID_by_pid() ->
	case ets:lookup(base_request, self()) of
		[#base_request_rec{id_mgw = ID} | _] ->
			ID;
		_ ->
			unknown
	end.

work_actions(ActionRequests) ->
	work_actions(ActionRequests, []).

work_actions([ActionRequest | ActionRequests], Ask) ->
	case work_context(ActionRequest) of
		Result when is_record(Result, 'ErrorDescriptor') ->
			{discard_ack, Result};
		[] ->
			ignore_trans_request;
		Result ->
			work_actions(ActionRequests, Ask ++ [Result])
	end;
work_actions([], Ask) ->
	case ets:lookup(base_request, self()) of
		[] ->
			ignore_trans_request;
		[Rec | _] when is_record(Rec, base_request_rec) ->
			case ets:lookup(base_mgw, Rec#base_request_rec.id_mgw) of
				[#base_mgw_rec{status = registration} | _] ->
					{{handle_ack, registration}, Ask};
				[#base_mgw_rec{status = online} | _] ->
					{discard_ack, Ask};
				[#base_mgw_rec{status = offline} | _] ->
					{discard_ack, Ask};
				_ ->
					ignore_trans_request
			end;
		_ ->
			ignore_trans_request
	end.

work_context(#'ActionRequest'{contextId = ?megaco_null_context_id, commandRequests = Commands}) ->
%%  пробегаемся по все командам в контексте
	case work_commands(?megaco_null_context_id, Commands) of
		{ok, Ask} ->
%%  TODO    применить команды
			#'ActionReply'{contextId = ?megaco_null_context_id, commandReply = Ask};
		{error, Ask} ->
%%  TODO    отменить команды
			#'ActionReply'{contextId = ?megaco_null_context_id, errorDescriptor = Ask, commandReply = []};
		_ ->
			[]
	end;
work_context(#'ActionRequest'{contextId = ?megaco_all_context_id}) ->
	#'ErrorDescriptor'{errorCode = ?megaco_incorrect_identifier, errorText = "*"};
work_context(#'ActionRequest'{contextId = ?megaco_choose_context_id}) ->
	#'ErrorDescriptor'{errorCode = ?megaco_incorrect_identifier, errorText = "$"};
work_context(#'ActionRequest'{contextId = Ctx, commandRequests = _Comands}) ->
%%    принадлежит контексту
	#'ErrorDescriptor'{
		errorCode = ?megaco_not_implemented,
		errorText = lists:append(["context = ", integer_to_list(Ctx), " "])
	}.

work_commands(Ctx, Commands) ->
	work_commands(Ctx, Commands, []).

work_commands(Ctx, [#'CommandRequest'{command = Command} | Commands], Ask) ->
	case work_command(Ctx, Command) of
		{ok, Result} ->
			work_commands(Ctx, Commands, Ask ++ [Result]);
		{error, Error} ->
			{error, Error}
	end;
work_commands(_Ctx, [], Ask) ->
	{ok, Ask}.

work_command(_Ctx, {serviceChangeReq, #'ServiceChangeRequest'{terminationID = TermID, serviceChangeParms = _Params}}) ->
%%  TODO    обработать команду
	ServiceChangeResult = {serviceChangeResParms,
		#'ServiceChangeResParm'{
			serviceChangeVersion = 1
		}
	},
	case TermID of
		[?megaco_root_termination_id] ->
			case get_ID_by_pid() of
				unknown ->
					{error, #'ErrorDescriptor'{
						errorCode = ?megaco_internal_gateway_error,
						errorText = "Pid not found"
					}};
				ID ->
					case ets:lookup(base_mgw, ID) of
						[] ->
							{error, #'ErrorDescriptor'{
								errorCode = ?megaco_internal_gateway_error,
								errorText = "MGW not found in base"
							}};
						[Rec | _] ->
							NewRec = Rec#base_mgw_rec{status = registration},
							ets:insert(base_mgw, NewRec),
							{ok, {serviceChangeReply, #'ServiceChangeReply'{
								terminationID = TermID,
								serviceChangeResult = ServiceChangeResult
							}}}
					end
			end;
		_ ->
			{error, #'ErrorDescriptor'{
				errorCode = ?megaco_not_implemented,
				errorText = "Only ROOT TID"
			}}
	end;
work_command(Ctx, {notifyReq, #'NotifyRequest'{terminationID = [TermID], observedEventsDescriptor = ObservedEvents}}) ->
	case TermID of
		?megaco_root_termination_id ->
			{error, #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "Root termination ID"}};
		#megaco_term_id{id = [?megaco_all]} ->
			{error, #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "* termination ID"}};
		#megaco_term_id{id = [?megaco_choose]} ->
			{error, #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "$ termination ID"}};
		#megaco_term_id{id = [TTermID | _]} ->
			case ets:lookup(?TABLE_REQUEST, self()) of
				[#base_request_rec{id_mgw = IdMGW, connHandle = ConnHandle} | _] ->
					try ets:lookup(IdMGW, string:to_lower(TTermID)) of
						[RecTid | _]->
%%							TODO проверить ID events и сам эвент
							erlang:spawn(service_line, start_talk, [ConnHandle, Ctx, RecTid, ObservedEvents]), %% запустить скрипт начала звонка
							{ok, {notifyReply, #'NotifyReply'{terminationID = [#megaco_term_id{id = [TTermID]}]}}};
						[] ->
%%							TODO ТИД не существует
							[]
					catch
						_:_ ->
							{error, #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "NotifyRequest"}}
					end;
				[] ->
					{error, #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "NotifyRequest"}}
			end
	end;

%%	#'ObservedEventsDescriptor'

%%	{error, #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "NotifyRequest"}};



work_command(_, _) ->
	{error, #'ErrorDescriptor'{errorCode = ?megaco_not_implemented, errorText = "Only ServiceChange or Notify"}}.


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