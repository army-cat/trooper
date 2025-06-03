-module(trooper_ssh_tests).
-author('manuel@altenwald.com').

-include_lib("eunit/include/eunit.hrl").

-define(USERNAME, "manuel.rubio").
-define(BASE_PATH, "_build/test/lib/trooper/test").

start_daemon() ->
    ok = ssh:start(),
    Opts = [
        {system_dir, ?BASE_PATH "/daemon1"},
        {user_dir, ?BASE_PATH "/user"}
    ],
    {ok, Sshd} = ssh:daemon(0, Opts),
    {ok, [{port, Port}|_]} = ssh:daemon_info(Sshd),
    {ok, Sshd, Port}.

stop_daemon(Sshd) ->
    ok = ssh:stop_daemon(Sshd),
    ok = ssh:stop(),
    ok.

transaction_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    ok = trooper_ssh:transaction(Opts, fun(Trooper) ->
        {ok,0,<<"3.141592653589793", _/binary>>} =
            trooper_ssh:exec(Trooper, "math:pi()."),
        ok
    end),
    ok = stop_daemon(Sshd),
    ok.

transaction_error_test() ->
    {ok, Sshd, Port} = start_daemon(),
    WOpts = [{host, "nxdomain_error"},
                {port, Port},
                {user, ?USERNAME},
                {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    {error, nxdomain} = trooper_ssh:transaction(WOpts, fun(_) -> ok end),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    ?assertException(throw, error, trooper_ssh:transaction(Opts, fun(_Trooper) ->
        throw(error)
    end)),
    ok = stop_daemon(Sshd),
    ok.

rsa_user_connect_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    {ok,0,<<"3.141592653589793", _/binary>>} =
        trooper_ssh:exec(Trooper, "math:pi()."),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.

rsa_user_with_args_connect_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    {ok,0,<<"3.141592653589793", _/binary>>} =
        trooper_ssh:exec(Trooper, "math:~p().", [pi]),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.

rsa_direct_user_connect_test() ->
    {ok, Sshd, Port} = start_daemon(),
    {ok, Cert} = file:read_file(?BASE_PATH "/user/id_rsa"),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, Cert}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    {ok,0,<<"3.141592653589793", _/binary>>} =
        trooper_ssh:exec(Trooper, "math:pi()."),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.

rsa_direct_error_user_connect_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, aaa}],
    {error,_} = trooper_ssh:start(Opts),
    ok = stop_daemon(Sshd),
    ok.

rsa_protected_user_connect_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa_protected"}},
            {rsa_pass_phrase, "secret"}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    {ok,0,<<"3.141592653589793", _/binary>>} =
        trooper_ssh:exec(Trooper, "math:pi()."),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd).

ecdsa_user_connect_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_ecdsa, {file, ?BASE_PATH "/user/id_ecdsa"}}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    {ok,0,<<"3.141592653589793", _/binary>>} =
        trooper_ssh:exec(Trooper, "math:pi()."),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.

ecdsa_protected_user_connect_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_ecdsa, {file, ?BASE_PATH "/user/id_ecdsa_protected"}},
            {ecdsa_pass_phrase, "secret"}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    {ok,0,<<"3.141592653589793", _/binary>>} =
        trooper_ssh:exec(Trooper, "math:pi()."),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd).

rsa_protected_error_user_connect_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa_protected"}}],
    {error,_} = trooper_ssh:start(Opts),
    ok = stop_daemon(Sshd).

file_error_user_connect_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/no_file"}}],
    {error,_} = trooper_ssh:start(Opts),
    ok = stop_daemon(Sshd).

-define(assertRegex(Pattern, Text), ?assertEqual(match, re:run(element(2, Text), Pattern, [{capture, none}]))).

shell_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}},
            {ptty_allow, true},
            {ptty_opts, [{term, "vt100"}]}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    PID = trooper_ssh:shell(Trooper),
    ?assert(is_pid(PID)),
    ?assertMatch({continue, <<"Eshell ", _/binary>>}, recv()),
    ?assertEqual({continue, <<"1> ">>}, recv()),
    
    PID ! {send, <<"io:format(\"hello world!\").\n">>},
    ?assertRegex("io:format\\(\"hello world!\"\\)\\.", recv()),
    ?assertRegex("1> io:format\\(\"hello world!\"\\)\\.", recv()),
    ?assertEqual({continue, <<"hello world!">>}, recv()),
    ?assertEqual({continue, <<"ok">>}, recv()),
    ?assertEqual({continue, <<"\r\n">>}, recv()),
    ?assertEqual({continue, <<"2> ">>}, recv()),

    PID ! {send, <<"exit().\n">>},
    ?assertRegex("exit\\(\\)\\.", recv()),
    ?assertRegex("2> exit\\(\\)\\.", recv()),

    ?assertEqual({exit_status, 0}, recv()),
    ?assertEqual(closed, recv()),
    ?assertNot(is_process_alive(PID)),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.

long_polling_exec_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    Cmd = "io:format(\"~b~n\", [",
    PID = trooper_ssh:exec_long_polling(Trooper, Cmd ++ "1])."),
    ?assert(is_pid(PID)),
    case list_to_integer(erlang:system_info(otp_release)) of
        N when N >= 22 ->
            %% only from OTP 22 we get the output from the
            %% command, in the rest, we get only the return
            %% of the function.
            {continue, <<"1", _/binary>>} = recv();
        _ ->
            ok
    end,
    {continue, <<"ok", _/binary>>} = recv(),
    {exit_status, 0} = recv(),
    closed = recv(),
    ?assertNot(is_process_alive(PID)),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.

long_polling_interactive_exec_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    Cmd = "io:get_line(\"give me data: \").",
    PID = trooper_ssh:exec_long_polling(Trooper, Cmd),
    ?assert(is_pid(PID)),
    PID ! {send, <<"1234\n">>},
    ?assertEqual({continue, <<"give me data: ">>}, recv()),
    ?assertEqual({continue, <<"1234\n">>}, recv()),
    ?assertEqual({exit_status, 0}, recv()),
    ?assertEqual(closed, recv()),
    ?assertNot(is_process_alive(PID)),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.

long_polling_exec_with_args_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    {ok, Trooper} = trooper_ssh:start(Opts),
    Cmd = "io:format(\"~~b~~n\", [~b]).",
    PID = trooper_ssh:exec_long_polling(Trooper, Cmd, [1]),
    ?assert(is_pid(PID)),
    case list_to_integer(erlang:system_info(otp_release)) of
        N when N >= 22 ->
            %% only from OTP 22 we get the output from the
            %% command, in the rest, we get only the return
            %% of the function.
            {continue, <<"1", _/binary>>} = recv();
        _ ->
            ok
    end,
    {continue, <<"ok", _/binary>>} = recv(),
    {exit_status, 0} = recv(),
    closed = recv(),
    ?assertNot(is_process_alive(PID)),
    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.

recv() ->
    receive
        {continue, Data} -> {continue, Data};
        Other -> Other
    after
        1000 -> {error, etimeout}
    end.
