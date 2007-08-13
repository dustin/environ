%%
%%
%%

-module(environ_utilities).
-export([get_therm_map/0, get_env_dict/1, get_env_dict/2,
	get_env/2, get_env/3, get_therm_name/1]).

% Get an application env var with a default
get_env(Which, Default) ->
	{ok, App} = application:get_application(),
	get_env(App, Which, Default).

get_env(App, Which, Default) ->
	case application:get_env(App, Which) of
		{ok, T} -> T;
		_ -> Default
	end.

% get a dict from an env list of key/value mappings
get_env_dict(Which) ->
	{ok, App} = application:get_application(),
	get_env_dict(App, Which).

% get a dict from an env list of key/value mappings
get_env_dict(App, Which) ->
	dict:from_list(get_env(App, Which, [])).

% Get the serial number -> name map for this application
get_therm_map() ->
	get_env_dict(temp_listener, therms).

% Look up a thermometer name by serial number
get_therm_name(SerialNumber) ->
	case dict:find(SerialNumber, get_therm_map()) of
		{ok, Name} -> Name;
		_ -> SerialNumber
	end.
