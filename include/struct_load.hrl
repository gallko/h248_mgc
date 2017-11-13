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
	apply_cmd = []  %% [{table_name, table_record}, ...]
}).

-record(base_line_rec, {
	id = 0  :: atom(),              %% '192.168.0.143:2944-A1'
	number  :: string(),            %% "89991893661"
	tid     :: string(),            %% "a1"
	id_mgw  :: atom(),              %% '192.168.0.143:2944' -> base_mgw_rec:id_rec
	
	service = true      :: boolean(),   %%
	eventID = 1         :: integer(),   %% id events -> base_mgw_events:id
	signalID= 0         :: integer(),   %% id signal -> base_mgw_signal:id
	
	none
}).