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
-export([log/2, log/3, inline_trace/1]).

%%----------------------------------------------------------------------
%% DEBUGGING
%%----------------------------------------------------------------------

log(warning, F) ->
	io:format("MGC[warning]: " ++ F);

log(notify, F) ->
	io:format("MGC[notify]: " ++ F);

log(error, F) ->
	io:format("MGC[error]: " ++ F);

log(debug, F) ->
	io:format("MGC[debug]: " ++ F).

log(warning, F, A) ->
	io:format("MGC[warning]: " ++ F, A);

log(notify, F, A) ->
	io:format("MGC[notify]: " ++ F, A);

log(error, F, A) ->
	io:format("MGC[error]: " ++ F, A);

log(debug, F, A) ->
	io:format("MGC[debug]: " ++ F, A).

%% -----------------------------------------------------------------------

inline_trace(true) ->
	megaco:enable_trace(max, io);
inline_trace(false) ->
	megaco:disable_trace();
inline_trace(_) ->
	ok.