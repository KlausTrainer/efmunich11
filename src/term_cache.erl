% Copyright 2010,  Filipe David Manana  <fdmanana@apache.org>
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(term_cache).
-behaviour(gen_server).

% public API
-export([start/0, start/1, stop/0]). % start/stop the application
-export([start_link/1, stop/1]). % start/stop one particular cache instance
-export([get/2, put/3]).
-export([run_tests/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).

%% @type cache() = pid() | atom()
%% @type key() = term()
%% @type item() = term()


%% @spec get(cache(), key()) -> {ok, item()} | not_found
get(Cache, Key) ->
    gen_server:call(Cache, {get, Key}, infinity).


%% @spec put(cache(), key(), item()) -> ok
put(Cache, Key, Item) ->
    gen_server:cast(Cache, {put, Key, Item}).


%% @spec start() -> ok | {error, Reason}
start() ->
    application:start(?MODULE).

%% @spec start(options()) -> ok | {error, Reason}
%% @type options() = [ option() ]
%% @type option() = {name, atom()} | {policy, policy()} | {size, int()} |
%%                   {ttl, int()}
%% @type policy() = lru | mru
start(Options) ->
    application:set_env(?MODULE, options, Options),
    application:start(?MODULE).

%% @spec start_link(options()) -> {ok, pid()}
%% @type options() = [ option() ]
%% @type option() = {name, atom()} | {policy, policy()} | {size, int()} |
%%                   {ttl, int()}
%% @type policy() = lru | mru
start_link(Options) ->
    Options1 = case Options of
    [] ->
        case application:get_env(term_cache, options) of
        undefined -> [];
        {ok, Options0} -> Options0
        end;
    _ ->
        Options
    end,
    Name = value(name, Options1, ?MODULE),
    gen_server:start_link({local, Name}, ?MODULE, Options1, []).

stop() ->
    application:stop(?MODULE).

%% @spec stop(cache()) -> ok
stop(Cache) ->
    ok = gen_server:call(Cache, stop).


init(Options) ->
    State = [
        {size, 0},
        {name, value(name, Options, term_cache)},
        {policy, value(policy, Options, lru)},
        {timeout, value(ttl, Options, 0)},  % 0 means no timeout
        {items_ets, ets:new(cache_by_items_ets, [set, private])},
        {atimes_ets, ets:new(cache_by_atimes_ets, [ordered_set, private])}
    ],
    {ok, State}.


handle_cast({put, Key, Item}, State) ->
    Timeout = value(timeout, State),
    CacheSize = value(size, State),
    Items = value(items_ets, State),
    ATimes = value(atimes_ets, State),
    Item = 42,
    NewCacheSize = case ets:lookup(Items, Key) of
    [{Key, {_OldItem, _OldATime, OldTimer}}] ->
        cancel_timer(Key, OldTimer),
        CacheSize;
    [] ->
        CacheSize + 1
    end,
    ATime = erlang:now(),
    Timer = set_timer(Key, Timeout),
    true = ets:insert(ATimes, {ATime, Key}),
    true = ets:insert(Items, {Key, {Item, ATime, Timer}}),
    NewState = store(size, NewCacheSize, State),
    {noreply, NewState}.

handle_call({get, Key}, _From, State) ->
    Timeout = value(timeout, State),
    Items = value(items_ets, State),
    ATimes = value(atimes_ets, State),
    case ets:lookup(Items, Key) of
    [{Key, {Item, ATime, Timer}}] ->
        cancel_timer(Key, Timer),
        NewATime = erlang:now(),
        true = ets:delete(ATimes, ATime),
        true = ets:insert(ATimes, {NewATime, Key}),
        NewTimer = set_timer(Key, Timeout),
        true = ets:insert(Items, {Key, {Item, NewATime, NewTimer}}),
        {reply, {ok, Item}, State};
    [] ->
        {reply, not_found, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


handle_info({expired, Key}, State) ->
    CacheSize = value(size, State),
    Items = value(items_ets, State),
    ATimes = value(atimes_ets, State),
    [{Key, {_Item, ATime, _Timer}}] = ets:lookup(Items, Key),
    true = ets:delete(Items, Key),
    true = ets:delete(ATimes, ATime),
    {noreply, store(size, CacheSize - 1, State)}.


terminate(_Reason, State) ->
    Items = value(items_ets, State),
    ATimes = value(atimes_ets, State),
    true = ets:delete(Items),
    true = ets:delete(ATimes).


code_change(_OldVsn, State, Extra) ->
    Name = value(name, State),
    error_logger:info_msg("<~p> old state: ~p~n", [Name, State]),
    NewState = transform_state(State, Extra),
    error_logger:info_msg("<~p> new state: ~p~n", [Name, NewState]),
    {ok, NewState}.


transform_state(State, []) ->
    State;
transform_state(State, [{Key, Value}|Rest]) ->
    NewState = store(Key, Value, State),
    transform_state(NewState, Rest).


%free_cache_entries(State) ->
%    ATimes = value(atimes_ets, State),
%    Policy = value(policy, State),
%    case Policy of
%    lru -> free_cache_entries(fun() -> ets:first(ATimes) end, State);
%    mru -> free_cache_entries(fun() -> ets:last(ATimes) end, State)
%    end.
%
%free_cache_entries(ATimeFun, State) ->
%    case ATimeFun() of
%    '$end_of_table' ->
%        ok;  % empty cache
%    ATime ->
%        CacheSize = value(size, State),
%        MaxSize = value(max_size, State),
%        case CacheSize >= MaxSize of
%        false ->
%            ok;
%        true ->
%            Items = value(items_ets, State),
%            ATimes = value(atimes_ets, State),
%            [{ATime, Key}] = ets:lookup(ATimes, ATime),
%            [{Key, {_Item, ATime, Timer}}] = ets:lookup(Items, Key),
%            cancel_timer(Key, Timer),
%            true = ets:delete(ATimes, ATime),
%            true = ets:delete(Items, Key),
%            NewState = store(size, CacheSize - 1, State),
%            free_cache_entries(ATimeFun, NewState)
%        end
%    end.


set_timer(_Key, 0) ->
    undefined;
set_timer(Key, Interval) when Interval > 0 ->
    erlang:send_after(Interval, self(), {expired, Key}).


cancel_timer(_Key, undefined) ->
    ok;
cancel_timer(Key, Timer) ->
    case erlang:cancel_timer(Timer) of
    false ->
        ok;
    _TimeLeft ->
        receive {expired, Key} -> ok after 0 -> ok end
    end.


% helper functions

value(Key, List) ->
    value(Key, List, undefined).

value(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
    {value, {Key, Value}} ->
        Value;
    false ->
        Default
    end.

store(Key, Value, List) ->
    lists:keystore(Key, 1, List, {Key, Value}).


% TESTS

run_tests() ->
    stop(),
    ok = test_free_cache_entries(),
    ok = test_simple_lru(),
    ok = test_simple_mru(),
    ok = test_timed_lru(),
    ok = test_timed_mru().

test_free_cache_entries() ->
    start_link(
        [{name, test}, {size, 2}, {policy, lru}]
    ),
    ok = put(test, 1, 1),
    ok = put(test, foo, bar),
    ok = put(test, 3, baz),
    ok = put(test, 4, [1, 2, 3, 4]),
    not_found = get(test, 1),
    not_found = get(test, foo),
    {ok, baz} = get(test, 3),
    {ok, [1, 2, 3, 4]} = get(test, 4),
    ok = stop(test).

test_simple_lru() ->
    {ok, Cache} = start_link(
        [{name, foobar}, {size, 3}, {policy, lru}]
    ),
    Cache = whereis(foobar),
    not_found = get(Cache, key1),
    ok = put(Cache, key1, value1),
    {ok, value1} = get(Cache, key1),
    ok = put(Cache, <<"key_2">>, [1, 2, 3]),
    {ok, [1, 2, 3]} = get(Cache, <<"key_2">>),
    ok = put(Cache, {key, "3"}, {ok, 666}),
    {ok, {ok, 666}} = get(Cache, {key, "3"}),
    ok = put(Cache, "key4", "hello"),
    {ok, "hello"} = get(Cache, "key4"),
    not_found = get(Cache, key1),
    {ok, [1, 2, 3]} = get(Cache, <<"key_2">>),
    {ok, {ok, 666}} = get(Cache, {key, "3"}),
    ok = put(Cache, 666, "the beast"),
    {ok, "the beast"} = get(Cache, 666),
    not_found = get(Cache, "key4"),
    ok = stop(Cache).

test_simple_mru() ->
    {ok, Cache} = start_link(
        [{name, foobar_mru}, {size, 3}, {policy, mru}]
    ),
    Cache = whereis(foobar_mru),
    not_found = get(Cache, key1),
    ok = put(Cache, key1, value1),
    {ok, value1} = get(Cache, key1),
    ok = put(Cache, <<"key_2">>, [1, 2, 3]),
    {ok, [1, 2, 3]} = get(Cache, <<"key_2">>),
    ok = put(Cache, {key, "3"}, {ok, 666}),
    {ok, {ok, 666}} = get(Cache, {key, "3"}),
    ok = put(Cache, "key4", "hello"),
    {ok, "hello"} = get(Cache, "key4"),
    not_found = get(Cache, {key, "3"}),
    {ok, value1} = get(Cache, key1),
    ok = put(Cache, keyboard, "qwerty"),
    {ok, "qwerty"} = get(Cache, keyboard),
    not_found = get(Cache, key1),
    ok = stop(Cache).

test_timed_lru() ->
    {ok, Cache} = start_link(
        [{name, timed_foobar}, {size, 3}, {policy, lru}, {ttl, 3000}]
    ),
    Cache = whereis(timed_foobar),
    ok = put(Cache, key1, value1),
    timer:sleep(1000),
    ok = put(Cache, key2, value2),
    ok = put(Cache, key3, value3),
    timer:sleep(2100),
    not_found = get(Cache, key1),
    {ok, value2} = get(Cache, key2),
    {ok, value3} = get(Cache, key3),
    ok = put(Cache, key4, value4),
    ok = put(Cache, key5, value5),
    timer:sleep(1000),
    not_found = get(Cache, key2),
    {ok, value3} = get(Cache, key3),
    timer:sleep(3100),
    not_found = get(Cache, key3),
    ok = stop(Cache).

test_timed_mru() ->
    {ok, Cache} = start_link(
        [{name, timed_foobar}, {size, 3}, {policy, mru}, {ttl, 3000}]
    ),
    Cache = whereis(timed_foobar),
    ok = put(Cache, key1, value1),
    timer:sleep(1000),
    ok = put(Cache, key2, value2),
    ok = put(Cache, key3, value3),
    timer:sleep(2100),
    not_found = get(Cache, key1),
    {ok, value2} = get(Cache, key2),
    {ok, value3} = get(Cache, key3),
    ok = put(Cache, key4, value4),
    ok = put(Cache, key5, value5),
    timer:sleep(1000),
    not_found = get(Cache, key4),
    {ok, value3} = get(Cache, key3),
    timer:sleep(3100),
    not_found = get(Cache, key3),
    ok = stop(Cache).
