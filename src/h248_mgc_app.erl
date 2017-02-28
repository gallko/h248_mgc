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
    h248_mgc_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Public functions
%%====================================================================
start() ->
    load_configure(),
    application:start(h248_mgc).

load_configure() ->
    megaco:start(),
    Res = file:get_cwd(),
    Res1 = file:consult("../priv/mgc.config"),
    false.