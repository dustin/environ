%%
%% Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
%%

-module(setup_schema).
-export([init_schema/0, init_tables/0]).

-include("env_alert.hrl").

db_nodes () ->
	[temp@purple].

init_schema() ->
	mnesia:create_schema(db_nodes()),
	ok = application:start(mnesia),
	init_tables(),
	ok = application:stop(mnesia).

init_tables() ->
	error_logger:info_msg("Creating alert_state", []),
	{atomic, ok} = mnesia:create_table(alert_state,
		[{disc_copies, db_nodes()},
		{attributes, record_info(fields, alert_state)}]),
	error_logger:info_msg("Creating therms", []),
	{atomic, ok} = mnesia:create_table(therms,
		[{disc_copies, db_nodes()},
		{attributes, record_info(fields, therms)}]),
	error_logger:info_msg("Done", []),
	true.
