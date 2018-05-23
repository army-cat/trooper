%% @doc Trooper Proxy Supervisor is in charge to start a supervisor under the
%%      `trooper_app' supervisor. This way the supervisor is reachable via the
%%      main supervisor in the application.
%%
%%      The supervisor is a simple one for one supervisor which is in charge to
%%      add chains and ssh connections to be supervised.
%%
%%      The advantage with this is when we want to stop the connection we can
%%      stop the supervisor and that's all. Even you can perform that task
%%      via `trooper_proxy:stop/1' function.
%% @end
-module(trooper_proxy_sup).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(supervisor).

-export([start/1, start_link/0, init/1, start_child/2]).

%% @private
start(Ref) ->
    ChildSpec = #{id => Ref,
                  start => {?MODULE, start_link, []},
                  restart => temporary,
                  shutdown => 5000,
                  type => supervisor,
                  modules => [?MODULE]},
    supervisor:start_child(trooper_app, ChildSpec).

%% @private
start_link() ->
    supervisor:start_link(?MODULE, []).

%% @private
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 1},
    ChildSpec = #{id => ssh,
                  start => {trooper_proxy_chain, start_link, []},
                  shutdown => brutal_kill},
    {ok, {SupFlags, [ChildSpec]}}.

%% @private
start_child(PID, Opts) ->
    supervisor:start_child(PID, Opts).
