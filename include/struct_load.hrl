%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Февр. 2017 16:08
%%%-------------------------------------------------------------------
-author("rus").

-type status_line_type() :: offline | online | in_talk.

-record(base_mgw_rec, {
	id_rec,                 %% '192.168.0.143:2944'
	mid_mgw,                %% {ip4Address,{'IP4Address',[192,168,0,81],2944}}
	mid_mgc,                %% {ip4Address,{'IP4Address',[192,168,0,81],2944}}
	count_port,             %% int
	status = offline,       %% online, offline, registration
	last_access = [],       %% local time - erlang:timestamp()
	inactivity_period = 10 * 1000000  %% microseconds
}).

-record(base_request_rec, {
	id,             %% pid
	id_mgw,         %% '192.168.0.143:2944'
	connHandle,     %% ConnHandle
	apply_cmd = []  %% [{table_name, table_record}, ...]
}).

-record(base_subscriber, {
	number  :: string(),    %% "89991893661"
	mgw_id  :: atom(),      %% '192.168.0.143:2944'
	tid     :: string()     %% "a1"
}).

-record(base_line_rec, {
%%	for each MGW is created new ets table
	tid     :: string(),            %% "a1"
	regScript :: string(),
	pid_awaiting :: pid(),
	table :: atom(), %% '192.168.0.143:2944'

	service = false     :: boolean(),   %%
	eventID = 1         :: integer(),   %% id events -> base_mgw_events:id
	signalID= "null"    :: string(),    %% id signal -> base_mgw_signal:id
	context = 0         :: integer(),   %% current context
	
	none
}).

-record(base_signals_rec, {
	id = nullsignal :: atom(),      %% atom-name signal
	signal = []     :: list()       %% [] | [{signal, #'Signal'{...}}]
}).

-record(base_events_rec, {
	id = 1          :: integer(),   %% number of events
	events = []     :: list()       %% list of #'RequestedEvent'
}).

-record(info_lua, {
	conn_handle,    %% #megaco_conn_handle
	record_tid      %% #base_line_rec
}).