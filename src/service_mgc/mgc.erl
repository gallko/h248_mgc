%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Сент. 2017 13:45
%%%-------------------------------------------------------------------
-module(mgc).
-author("rus").

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

%% API
-export([
	add_user/0,
	add_user/4,
	add_transport/3]).

%%%===================================================================
%%% Public functions
%%%===================================================================

add_user(Ip, Port, Script, _Config) when is_list(Ip) and is_integer(Port) and is_atom(Script) ->
	Addr = case inet:parse_address(Ip) of
		       {ok, A} ->
			       tuple_to_list(A);
		       {error, einval} ->
			       log:log(warning, "ip not correct, set mid user 127.0.0.1~n"),
			       [127, 0, 0, 1]
	       end,
	Mid = {ip4Address, #'IP4Address'{address = Addr, portNumber = Port}},
	start_user(Mid, Script, []);

add_user(_Ip, _Port, _Script, _Config) ->
	log:log(warning, "value not correct, set by default ~n"
	"   IP:     127.0.0.1~n"
	"   Port:   2944"
	"   Script: callback~n"),
	Mid = {ip4Address, #'IP4Address'{address = {127, 0, 0, 1}, portNumber = 2944}},
	start_user(Mid, callback, []).

add_user() ->
	log:log(warning, "value not correct, set by default ~n"
	"   IP:     127.0.0.1~n"
	"   Port:   2944"
	"   Script: callback~n"),
	Mid = #'IP4Address'{address = {127, 0, 0, 1}, portNumber = 2944},
	start_user(Mid, callback, []).

add_transport({ip4Address, Mid}, Encoding, Protocol) when
	is_record(Mid, 'IP4Address') and
		((Protocol == megaco_tcp) or (Protocol == megaco_udp)) and
		((Encoding == megaco_pretty_text_encoder) or (Encoding == megaco_compact_text_encoder)) ->
	RecHandle = megaco:user_info({ip4Address, Mid}, receive_handle),
	NewHandle = RecHandle#megaco_receive_handle{encoding_mod = Encoding,
		encoding_config = [],
		send_mod = Protocol},
	log:log(debug, "RecHandle: ~p~n", [NewHandle]),
	start_transport(Mid#'IP4Address'.portNumber, NewHandle);

add_transport(_Mid, _Encoding, _Protocol) ->
	[].


%%%===================================================================
%%% Private functions
%%%===================================================================

start_user(Mid, Script, Config) ->
	case megaco:start_user(Mid, [{user_mod, Script} | Config]) of
		ok ->
			log:log(notify, "user startet whith mid: ~p~n", [Mid]),
			Mid;
		{error, Reason} ->
			log:log(error, "user startet failed whith mid: ~p~n Reason: ~p~n", [Mid, Reason])
	end
.

start_transport(MgcPort, RecHandle) ->
	case RecHandle#megaco_receive_handle.send_mod of
		megaco_tcp -> start_tcp(MgcPort, RecHandle);
		megaco_udp -> start_udp(MgcPort, RecHandle);
		SendMod -> {error, {bad_send_mod, SendMod}}
	end.

start_tcp(_MgcPort, _RecHandle) ->
	[].

start_udp(MgcPort, RecHandle) ->
	case megaco_udp:start_transport() of
		{ok, SupPid} ->
			Options = [{port, MgcPort}, {receive_handle, RecHandle}],
			case megaco_udp:open(SupPid, Options) of
				{ok, _SendHandle, _ControlPid} ->
					ok;
				{error, Reason} ->
					{error, {megaco_udp_open, Reason}}
			end;
		{error, Reason} ->
			log:log(error, "user startet udp whith RecHandle: ~p~n Reason: ~p~n", [RecHandle, Reason])
	end,
	[].


%%loadConfig() ->
%%    {ok, Dir} = file:get_cwd(),
%%    File = Dir ++ "/../" ++ ?CONFIG_FILE,
%%    case file:consult(File) of
%%        {ok, Result} ->
%%            log:log(notify, "load configure from file:~n    ~p~n", [Dir ++ "/../" ++ File]),
%%            [parsingConfig(Conf) || Conf <- Result];
%%        {error, _} ->
%%            log:log(warning, "configure file not found~n"),
%%            error
%%    end.
%%
%%parsingConfig({mgc, Params}) ->
%%    [];
%%
%%parsingConfig({loger, Params}) ->
%%    [];
%%
%%parsingConfig(_NotImplemented) ->
%%    [].
