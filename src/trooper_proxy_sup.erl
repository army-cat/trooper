-module(trooper_proxy_sup).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(supervisor).

-export([start/1, start_link/0, init/1, start_child/2]).

start(Ref) ->
    ChildSpec = #{id => Ref,
                  start => {?MODULE, start_link, []},
                  restart => temporary,
                  shutdown => 5000,
                  type => supervisor,
                  modules => [?MODULE]},
    supervisor:start_child(trooper_app, ChildSpec).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 1},
    ChildSpec = #{id => ssh,
                  start => {trooper_proxy_chain, start_link, []},
                  shutdown => brutal_kill},
    {ok, {SupFlags, [ChildSpec]}}.

start_child(PID, Opts) ->
    supervisor:start_child(PID, Opts).
