%%
%% arch-tag: 280D71B6-9F20-11D8-ACEE-000A957659CC
%%

-module(environ_utilities).
-export([get_therm_map/0]).

% Get the serial number -> name map for this application
get_therm_map() ->
	Therms = case application:get_env(therms) of
		{ok, T} -> T;
		_ -> []
	end,
	lists:foldl(fun ({K, V}, Acc) ->
			dict:update(K, fun(_) -> V end, V, Acc)
		end,
		dict:new(), Therms).

