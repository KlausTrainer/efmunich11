-module(term_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    start_link([]).

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).

init(Options) ->
    TermCache = {term_cache, {term_cache, start_link, Options},
                 permanent, brutal_kill, worker, [term_cache]},
    Children = [TermCache],
    RestartStrategy = {one_for_one, 5, 4},
    {ok, {RestartStrategy, Children}}.
