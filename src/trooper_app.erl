%% @doc Trooper Application is in charge of start the application, dependencies
%%      configure the main supervisor where all of the other supervisors will
%%      be attached.
%% @end
-module(trooper_app).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

%% @private
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
stop(_State) ->
    ok.

%% @private
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 1},
    {ok, {SupFlags, []}}.
