-module(term_cache_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

start(_Type, Options) ->
    term_cache_sup:start_link(Options).

stop(_State) ->
    ok.
