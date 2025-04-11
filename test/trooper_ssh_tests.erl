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

transaction_test_() ->
    {timeout, 10, ?_test(begin
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
        ok
    end)}.

transaction_error_test_() ->
    {timeout, 10, ?_test(begin
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
        ok
    end)}.

rsa_user_connect_test_() ->
    {timeout, 10, ?_test(begin
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
        ok
    end)}.

rsa_user_with_args_connect_test_() ->
    {timeout, 10, ?_test(begin
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
        ok
    end)}.

rsa_direct_user_connect_test_() ->
    {timeout, 10, ?_test(begin
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
        ok
    end)}.

rsa_direct_error_user_connect_test_() ->
    {timeout, 10, ?_test(begin
        {ok, Sshd, Port} = start_daemon(),
        Opts = [{host, "localhost"},
                {port, Port},
                {user, ?USERNAME},
                {id_rsa, aaa}],
        {error,_} = trooper_ssh:start(Opts),
        ok = stop_daemon(Sshd),
        ok
    end)}.

rsa_protected_user_connect_test_() ->
    {timeout, 10, fun() ->
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
        ok = stop_daemon(Sshd)
    end}.

dsa_user_connect_test_() ->
    case list_to_integer(erlang:system_info(otp_release)) < 23 of
        true ->
            {timeout, 10, ?_test(begin
                {ok, Sshd, Port} = start_daemon(),
                Opts = [{host, "localhost"},
                        {port, Port},
                        {user, ?USERNAME},
                        {id_dsa, {file, ?BASE_PATH "/user/id_dsa"}}],
                {ok, Trooper} = trooper_ssh:start(Opts),
                {ok,0,<<"3.141592653589793", _/binary>>} =
                    trooper_ssh:exec(Trooper, "math:pi()."),
                ok = trooper_ssh:stop(Trooper),
                ok = stop_daemon(Sshd),
                ok
            end)};
        false ->
            %% FIXME: Ok, maybe it's a configuration possible to get this
            %% working with DSA... but it's too difficult at the moment.
            fun() -> ok end
    end.


dsa_protected_user_connect_test_() ->
    case list_to_integer(erlang:system_info(otp_release)) < 23 of
        true ->
            {timeout, 10, fun() ->
                {ok, Sshd, Port} = start_daemon(),
                Opts = [{host, "localhost"},
                        {port, Port},
                        {user, ?USERNAME},
                        {id_dsa, {file, ?BASE_PATH "/user/id_dsa_protected"}},
                        {dsa_pass_phrase, "secret"}],
                {ok, Trooper} = trooper_ssh:start(Opts),
                {ok,0,<<"3.141592653589793", _/binary>>} =
                    trooper_ssh:exec(Trooper, "math:pi()."),
                ok = trooper_ssh:stop(Trooper),
                ok = stop_daemon(Sshd)
            end};
        false ->
            %% FIXME: Ok, maybe it's a configuration possible to get this
            %% working with DSA... but it's too dificult at the moment.
            fun() -> ok end
    end.

ecdsa_user_connect_test_() ->
    {timeout, 10, ?_test(begin
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
        ok
    end)}.

ecdsa_protected_user_connect_test_() ->
    {timeout, 10, fun() ->
        {ok, Sshd, Port} = start_daemon(),
        Opts = [{host, "localhost"},
                {port, Port},
                {user, ?USERNAME},
                {id_ecdsa, {file, ?BASE_PATH "/user/id_ecdsa_protected"}},
                {dsa_pass_phrase, "secret"}],
        {ok, Trooper} = trooper_ssh:start(Opts),
        {ok,0,<<"3.141592653589793", _/binary>>} =
            trooper_ssh:exec(Trooper, "math:pi()."),
        ok = trooper_ssh:stop(Trooper),
        ok = stop_daemon(Sshd)
    end}.

rsa_protected_error_user_connect_test_() ->
    {timeout, 10, ?_test(begin
        {ok, Sshd, Port} = start_daemon(),
        Opts = [{host, "localhost"},
                {port, Port},
                {user, ?USERNAME},
                {id_rsa, {file, ?BASE_PATH "/user/id_rsa_protected"}}],
        {error,_} = trooper_ssh:start(Opts),
        ok = stop_daemon(Sshd)
    end)}.

file_error_user_connect_test_() ->
    {timeout, 10, ?_test(begin
        {ok, Sshd, Port} = start_daemon(),
        Opts = [{host, "localhost"},
                {port, Port},
                {user, ?USERNAME},
                {id_rsa, {file, ?BASE_PATH "/user/no_file"}}],
        {error,_} = trooper_ssh:start(Opts),
        ok = stop_daemon(Sshd)
    end)}.

long_polling_exec_test_() ->
    {timeout, 10, ?_test(begin
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
        ok
    end)}.

%% FIXME: trying to figure out how to make this working :'(
% long_polling_interactive_exec_test_() ->
%     {timeout, 10, ?_test(begin
%         {ok, Sshd, Port} = start_daemon(),
%         Opts = [{host, "localhost"},
%                 {port, Port},
%                 {user, ?USERNAME},
%                 {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
%         {ok, Trooper} = trooper_ssh:start(Opts),
%         Cmd = "io:get_line(\"give me data: \").",
%         PID = trooper_ssh:exec_long_polling(Trooper, Cmd),
%         ?assert(is_pid(PID)),
%         PID ! {send, <<"1234\n">>},
%         {continue, <<"give me data: ">>} = recv(),
%         {continue, <<"1234\n">>} = recv(),
%         {exit_status, 0} = recv(),
%         closed = recv(),
%         ?assertNot(is_process_alive(PID)),
%         ok = trooper_ssh:stop(Trooper),
%         ok = stop_daemon(Sshd),
%         ok
%     end)}.

long_polling_exec_with_args_test_() ->
    {timeout, 10, ?_test(begin
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
        ok
    end)}.

recv() ->
    receive
        {continue, Data} -> {continue, Data};
        Other -> Other
    after
        1000 -> {error, etimeout}
    end.
