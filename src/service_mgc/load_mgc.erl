%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Март 2017 8:27
%%%-------------------------------------------------------------------
-module(load_mgc).
-author("rus").

-behaviour(gen_server).

%% Include
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-include("define_mgc.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% public function
-export([add_user/4, add_user/0, add_transport/3]).

-define(SERVER, ?MODULE).

-record(state, {id}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%    loop().

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    log:log(debug, "init [~p:~p]~n", [?MODULE, ?LINE]),
    loadConfig(),
    {ok, #state{id = 1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, _State) ->
    log:log(debug, "terminate by reason ~p [~p:~p]~n", [Reason, ?MODULE, ?LINE]),
    Reason.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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

add_transport(Mid, Encoding, Protocol) ->
    [].



%%%===================================================================
%%% Private functions
%%%===================================================================

start_user(Mid, Script, Config) ->
    case megaco:start_user(Mid, [{user_mod, Script} | Config])  of
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
        SendMod    -> {error, {bad_send_mod, SendMod}}
    end.

start_tcp(MgcPort, RecHandle) ->
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

loadConfig() ->
    {ok, Dir} = file:get_cwd(),
    File = Dir ++ "/../" ++ ?CONFIG_FILE,
    case file:consult(File) of
        {ok, Result} ->
            log:log(notify, "load configure from file:~n    ~p~n", [Dir ++ "/../" ++ File]),
            [parsingConfig(Conf) || Conf <- Result];
        {error, _} ->
            log:log(warning, "configure file not found~n"),
            error
    end.

parsingConfig({mgc, Params}) ->
    [];

parsingConfig({loger, Params}) ->
    [];

parsingConfig(_NotImplemented) ->
    [].
