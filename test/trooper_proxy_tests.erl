-module(trooper_proxy_tests).
-author('manuel@altenwald.com').

-include_lib("eunit/include/eunit.hrl").

-define(USERNAME, "manuel.rubio").
-define(BASE_PATH, "../test").

start_daemon(N) ->
    ok = ssh:start(),
    Opts = [
        {system_dir, ?BASE_PATH "/daemon" ++ N},
        {user_dir, ?BASE_PATH "/user"}
    ],
    {ok, Sshd} = ssh:daemon(0, Opts),
    {ok, [{port, Port}]} = ssh:daemon_info(Sshd),
    {ok, Sshd, Port}.

stop_daemon(Sshd) ->
    ok = ssh:stop_listener(Sshd),
    ok = ssh:stop_daemon(Sshd),
    ok.

%% the command "nc" is not possible to be used with Erlang SSH Server
%% we have to configure real SSH servers to perform this test.
%%
% rsa_user_connect_test() ->
%     {ok, Sshd1, Port1} = start_daemon("1"),
%     {ok, Sshd2, Port2} = start_daemon("2"),
%     Opts = [[{host, "localhost"},
%              {port, Port1},
%              {user, ?USERNAME},
%              {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}},
%              {proxy, "os:cmd(\"nc ~s ~b\")."}],
%             [{host, "localhost"},
%              {port, Port2},
%              {user, ?USERNAME},
%              {id_dsa, {file, ?BASE_PATH "/user/id_dsa"}},
%              {proxy, "os:cmd(\"nc ~s ~b\")."}]],
%     {ok, Trooper} = trooper_proxy:start(Opts),
%     Cmd = "whereis(ssh_system_any_~b_default_sup).",
%     {ok, 0, <<"<",_/binary>>} = trooper_ssh:exec(Trooper, Cmd, [Port2]),
%     ok = trooper_ssh:stop(Trooper),
%     ok = stop_daemon(Sshd2),
%     ok = stop_daemon(Sshd1),
%     ok.
