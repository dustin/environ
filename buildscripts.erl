%%
%% arch-tag: C2451093-9F1E-11D8-B93C-000A957659CC
%%

-module(buildscripts).
-export([makeboot/0, maketar/0]).

makeboot() ->
	io:format("Making boot~n", []),
	systools:make_script("environ", [local]).

maketar() ->
	io:format("Making tar~n", []),
	systools:make_tar("environ").
