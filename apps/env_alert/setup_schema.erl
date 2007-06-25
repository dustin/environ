%%
%% Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
%%

-module(setup_schema).
-export([init_schema/0, init_tables/0]).

-include("env_alert.hrl").

db_nodes () ->
	[temp@rubik, 'temp@dhcp-111'].

init_schema() ->
	mnesia:create_schema(db_nodes()).

init_tables() ->
	{atomic, ok} = mnesia:create_table(alert_state,
		[{disc_copies, db_nodes()},
		{attributes, record_info(fields, alert_state)}]),
	{atomic, ok} = mnesia:create_table(therms,
		[{disc_copies, db_nodes()},
		{attributes, record_info(fields, therms)}]),
	true.
