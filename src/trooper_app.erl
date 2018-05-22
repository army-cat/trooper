-module(trooper_app).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 1},
    {ok, {SupFlags, []}}.
