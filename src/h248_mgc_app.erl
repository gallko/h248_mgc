%%%-------------------------------------------------------------------
%% @doc h248_mgc public API
%% @end
%%%-------------------------------------------------------------------

-module(h248_mgc_app).

-behaviour(application).

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

    timer:sleep(timer:seconds(1)),
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
    Mid_MGC = load_mgc:add_user("192.168.1.103", 2944, callback, []),
    load_mgc:add_transport(Mid_MGC, megaco_pretty_text_encoder, megaco_udp),
    mgw_sup:add_mgw(Mid_MGC, "192.168.1.108").

loop() ->
    receive
        _Msg ->
            []
    end,
    loop().