%%
%% arch-tag: 0D3434E9-A3CF-11D8-B789-000A957659CC
%%

-module(gen_sup).

-behavior(supervisor).

-export([init/1]).

% Generic supervisor initialization
init([Rv|Args]) -> {ok, Rv}.
