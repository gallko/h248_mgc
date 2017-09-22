%%%-------------------------------------------------------------------
%%% @author rus
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Февр. 2017 16:08
%%%-------------------------------------------------------------------
-author("rus").

-record(base_mgw_rec, {
    name,                   %% '192.168.0.143:2944'
    mid_mgw,                %% {ip4Address,{'IP4Address',[192,168,0,81],2944}}
    mid_mgc,                %% {ip4Address,{'IP4Address',[192,168,0,81],2944}}
    count_port,             %% int
    status = offline,       %% online, offline, registration
    last_access = [],       %% local time
    inactivity_period = 10 * 1000000  %% microseconds
}).

-record(base_request_rec, {
    id,             %% pid
    id_mgw,         %% '192.168.0.143:2944'
    apply_cmd = []  %% [{table_name, table_record}, ...]
}).

-record(base_line, {

}).