-module(trooper_proxy_tests).
-author('manuel@altenwald.com').

-include_lib("eunit/include/eunit.hrl").

-define(USERNAME, "manuel.rubio").
-define(BASE_PATH, "_build/test/lib/trooper/test").

exec_fun(Cmd) ->
    try
        {ok, Tokens, _} = erl_scan:string(Cmd),
        {ok, Exprs} = erl_parse:parse_exprs(Tokens),
        {value, Val, _} = erl_eval:exprs(Exprs, erl_eval:new_bindings()),
        case io_lib:printable_unicode_list(Val) of
            true -> {ok, Val};
            false -> {ok, io_lib:format("~p", [Val])}
        end
    catch
        _:_ -> {ok, Cmd}
    end.

start_daemon() ->
    ok = ssh:start(),
    Opts = [
        {system_dir, ?BASE_PATH "/daemon1"},
        {user_dir, ?BASE_PATH "/user"},
        {exec, {direct, fun exec_fun/1}}
    ],
    {ok, Sshd} = ssh:daemon(0, Opts),
    {ok, [{port, Port}|_]} = ssh:daemon_info(Sshd),
    {ok, Sshd, Port}.

stop_daemon(Sshd) ->
    ok = ssh:stop_daemon(Sshd),
    ok = ssh:stop(),
    ok.

single_hop_proxy_test() ->
    {ok, _} = application:ensure_all_started(trooper),
    {ok, Sshd, Port} = start_daemon(),
    Opts = [[{host, "localhost"},
             {port, Port},
             {user, ?USERNAME},
             {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}]],
    {ok, TProxy} = trooper_proxy:start(Opts),

    %% Test exec/2
    {ok, 0, <<"3.14", _/binary>>} = trooper_proxy:exec(TProxy, "math:pi()."),

    %% Test exec/3 with format
    {ok, 0, <<"hello", _/binary>>} = trooper_proxy:exec(TProxy, "\"~s\".", ["hello"]),

    %% Test exec_long_polling/2
    PID1 = trooper_proxy:exec_long_polling(TProxy, "math:pi()."),
    ?assert(is_pid(PID1)),
    {continue, <<"3.14", _/binary>>} = recv(),
    {exit_status, 0} = recv(),
    closed = recv(),
    wait_dead(PID1),

    %% Test exec_long_polling/3
    PID2 = trooper_proxy:exec_long_polling(TProxy, "\"~s\".", ["hello"]),
    ?assert(is_pid(PID2)),
    {continue, <<"hello", _/binary>>} = recv(),
    {exit_status, 0} = recv(),
    closed = recv(),
    wait_dead(PID2),

    ok = trooper_proxy:stop(TProxy),
    ok = stop_daemon(Sshd),
    ok.

proxy_sup_test() ->
    {ok, _} = application:ensure_all_started(trooper),
    Ref = make_ref(),
    {ok, SupPid} = trooper_proxy_sup:start(Ref),
    ?assert(is_pid(SupPid)),
    ok = supervisor:terminate_child(trooper_app, Ref),
    ok.

proxy_chain_processing_test() ->
    {ok, _} = application:ensure_all_started(trooper),
    {ok, Sshd, Port} = start_daemon(),
    Config = [{host, "localhost"},
              {port, Port},
              {user, ?USERNAME},
              {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    Cmd = "io:format(\"~s ~b~n\").",
    {ok, BridgePid} = trooper_proxy_chain:start_link(self(), Config, Cmd, "localhost", Port),
    LocalPort = receive {port, P} -> P after 1000 -> error(timeout) end,
    {ok, ClientSocket} = gen_tcp:connect("127.0.0.1", LocalPort, [binary, {active, true}]),
    ok = gen_tcp:send(ClientSocket, <<"hello from tcp\n">>),
    receive
        {tcp, ClientSocket, <<"hello from tcp\n">>} -> ok
    after 500 ->
        ok
    end,
    ok = gen_tcp:close(ClientSocket),
    unlink(BridgePid),
    exit(BridgePid, shutdown),
    ok = stop_daemon(Sshd),
    ok.

proxy_chain_single_init_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Config = [{host, "localhost"},
              {port, Port},
              {user, ?USERNAME},
              {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    {ok, BridgePid} = trooper_proxy_chain:start_link(self(), Config),
    receive
        {trooper, Trooper} ->
            {ok, 0, <<"3.14", _/binary>>} = trooper_ssh:exec(Trooper, "math:pi()."),
            ok = trooper_ssh:stop(Trooper)
    after 1000 ->
        error(timeout)
    end,
    unlink(BridgePid),
    exit(BridgePid, shutdown),
    ok = stop_daemon(Sshd),
    ok.

proxy_chain_terminate_test() ->
    ok = trooper_proxy_chain:terminate(normal, undefined),
    ok.

proxy_errors_test() ->
    {ok, _} = application:ensure_all_started(trooper),
    Opts = [[{host, "unreachable_host_error"},
             {port, 12345},
             {user, ?USERNAME},
             {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}]],
    ?assertException(throw, {error, _}, trooper_proxy:start(Opts)),
    ok.

multi_hop_proxy_start_test() ->
    {ok, _} = application:ensure_all_started(trooper),
    {ok, Sshd, Port} = start_daemon(),
    Opts = [
        [{host, "localhost"},
         {port, Port},
         {user, ?USERNAME},
         {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}},
         {proxy, "io:format(\"~s ~b~n\")."}],
        [{host, "localhost"},
         {port, Port},
         {user, ?USERNAME},
         {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}]
    ],
    ?assertException(throw, {error, _}, trooper_proxy:start(Opts)),
    ok = stop_daemon(Sshd),
    ok.

wait_dead(PID) ->
    MRef = monitor(process, PID),
    receive
        {'DOWN', MRef, process, PID, _} -> ok
    after 1000 ->
        error({process_still_alive, PID})
    end.

recv() ->
    receive
        {continue, Data} -> {continue, Data};
        Other -> Other
    after
        1000 -> {error, etimeout}
    end.
