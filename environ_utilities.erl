%%
%% arch-tag: 280D71B6-9F20-11D8-ACEE-000A957659CC
%%

-module(environ_utilities).
-export([get_therm_map/0, get_env_dict/1, get_env/2]).

% Get an application env var with a default
get_env(Which, Default) ->
	case application:get_env(Which) of
		{ok, T} -> T;
		_ -> Default
	end.

% get a dict from an env list of key/value mappings
get_env_dict(Which) ->
	lists:foldl(fun ({K, V}, Acc) ->
			dict:update(K, fun(_) -> V end, V, Acc)
		end,
		dict:new(), get_env(Which, [])).

% Get the serial number -> name map for this application
get_therm_map() ->
	get_env_dict(therms).

