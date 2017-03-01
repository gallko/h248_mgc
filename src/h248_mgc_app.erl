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
    megaco:start(),
    application:start(h248_mgc).

load_configure() ->
    case file:consult("../priv/mgc.configs") of
        {ok, Settings} ->
            _Result = [parser_setting(Set) || Set <- Settings],
%%            {ok, Open} = file:open("copy.txt", write),
            _Result = file:write_file("copy.txt", io_lib:fwrite("~p.\n",[Settings])),
%%            file:close(Open),
            true;
        _ ->
            false
    end.

parser_setting(_Setting) ->
    true.