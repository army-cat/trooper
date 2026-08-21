-module(trooper_app_tests).
-author('manuel@altenwald.com').

-include_lib("eunit/include/eunit.hrl").

app_test() ->
    {ok, _} = application:ensure_all_started(trooper),
    ?assertNotEqual(undefined, whereis(trooper_app)),
    ok = application:stop(trooper),
    ok.

init_test() ->
    {ok, {#{strategy := one_for_one}, []}} = trooper_app:init([]),
    ok = trooper_app:stop(undefined),
    ok.
