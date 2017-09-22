%%%-------------------------------------------------------------------
%% @doc h248_mgc public API
%% @end
%%%-------------------------------------------------------------------

-module(h248_mgc_app).

-behaviour(application).

-include("../include/struct_load.hrl").

%% Public
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    log:log(notify, "start application [~p]~n", [?MODULE]),
    megaco:start(),
    megaco:enable_trace(min, io),
    Return = h248_mgc_sup:start_link(),

%%    init table
    ets:new(base_mgw, [ordered_set, public, named_table, {keypos, #base_mgw_rec.name}]),
    ets:new(base_request, [ordered_set, public, named_table, {keypos, #base_request_rec.id}]),

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
    case mgw:add(Mid_MGC, "192.168.0.143", 72) of
        {ok, _MGw} ->
            [];
        {error, _} ->
            []
    end,
    case mgw:add(Mid_MGC, "192.168.0.145", 72) of
        {ok, _MGw1} ->
            [];
        {error, _} ->
            []
    end.


loop() ->
    receive
        _Msg ->
            []
    end,
    loop().