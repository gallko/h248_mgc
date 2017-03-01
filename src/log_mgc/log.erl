%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Март 2017 9:19
%%%-------------------------------------------------------------------
-module(log).
-author("rus").

%% API
-export([log/1, log/2, inline_trace/1]).

%%----------------------------------------------------------------------
%% DEBUGGING
%%----------------------------------------------------------------------

log(F) ->
    log(F, []).

log(F,A) ->
    d(get(debug),F,A).

d(true,F,A) ->
    io:format("MGC: " ++ F ++ "~n", A);
d(_, _F, _A) ->
    ok.

%% -----------------------------------------------------------------------

inline_trace(true) ->
    megaco:enable_trace(max, io);
inline_trace(false) ->
    megaco:disable_trace();
inline_trace(_) ->
    ok.