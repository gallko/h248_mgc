%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Февр. 2017 16:21
%%%-------------------------------------------------------------------
-author("rus").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

%% name of tables
-define(TABLE_MGW, base_mgw).           %% данные о MGW
-define(TABLE_REQUEST, base_request).   %% данные о запросе, используется в request
-define(TABLE_SUBSCRIBER, base_number). %% данные об абоненте, number of telephone, MGW_ID, TermID
-define(TABLE_SIGNALS, base_signals).   %% список сигналов                              #base_signals_rec
-define(TABLE_EVENTS, base_events).     %% список событий на которе подписывается SSW   #base_events_rec

%% default define
-define(DEV, "eth1").
-define(IP, [192, 168, 0, 81]).
-define(TRANSPORT, "udp").
-define(CODING, "pretty").
-define(CONFIG_FILE, "priv/mgc.config").

-define(PERIOD_ASK_MGW, 60). %% период опроса
-define(TIMER_MEGACO_ASK, #megaco_incr_timer{wait_for = 200, factor = 2, incr = 0, max_retries = 5}).

%%-define(MG1_MID, {ip4Address, #'IP4Address'{address = [127,0,0,1], portNumber = 2944}}).

