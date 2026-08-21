-module(trooper_scp_tests).
-author('manuel@altenwald.com').

-include_lib("eunit/include/eunit.hrl").

-define(USERNAME, "manuel.rubio").
-define(BASE_PATH, "_build/test/lib/trooper/test").

start_daemon() ->
    ok = ssh:start(),
    Opts = [
        {system_dir, ?BASE_PATH "/daemon1"},
        {user_dir, ?BASE_PATH "/user"},
        {subsystems, [ssh_sftpd:subsystem_spec([{root, ?BASE_PATH}])]}
    ],
    {ok, Sshd} = ssh:daemon(0, Opts),
    {ok, [{port, Port}|_]} = ssh:daemon_info(Sshd),
    {ok, Sshd, Port}.

stop_daemon(Sshd) ->
    ok = ssh:stop_daemon(Sshd),
    ok = ssh:stop(),
    ok.

scp_operations_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    {ok, Trooper} = trooper_ssh:start(Opts),

    %% Test write_file & read_file
    TestFile = "/test_file.txt",
    TestContent = <<"Hello Trooper SFTP!">>,
    ok = trooper_scp:write_file(Trooper, TestFile, TestContent),
    {ok, ReadContent} = trooper_scp:read_file(Trooper, TestFile),
    ?assertEqual(TestContent, ReadContent),

    %% Test list_dir
    {ok, Files} = trooper_scp:list_dir(Trooper, "/"),
    ?assert(lists:member("test_file.txt", Files)),

    %% Test rename
    RenamedFile = "/test_file_renamed.txt",
    ok = trooper_scp:rename(Trooper, TestFile, RenamedFile),
    {ok, Files2} = trooper_scp:list_dir(Trooper, "/"),
    ?assert(lists:member("test_file_renamed.txt", Files2)),
    ?assertNot(lists:member("test_file.txt", Files2)),

    %% Test delete
    ok = trooper_scp:delete(Trooper, RenamedFile),
    {ok, Files3} = trooper_scp:list_dir(Trooper, "/"),
    ?assertNot(lists:member("test_file_renamed.txt", Files3)),

    %% Test make_dir & del_dir
    TestDir = "/test_dir",
    ok = trooper_scp:make_dir(Trooper, TestDir),
    {ok, Files4} = trooper_scp:list_dir(Trooper, "/"),
    ?assert(lists:member("test_dir", Files4)),
    ok = trooper_scp:del_dir(Trooper, TestDir),

    %% Test symlink
    _ = trooper_scp:delete(Trooper, "/link.txt"),
    _ = trooper_scp:delete(Trooper, "/source.txt"),
    ok = trooper_scp:write_file(Trooper, "/source.txt", <<"source data">>),
    ok = trooper_scp:make_symlink(Trooper, "/link.txt", "/source.txt"),
    {ok, FilesLink} = trooper_scp:list_dir(Trooper, "/"),
    ?assert(lists:member("link.txt", FilesLink)),
    ok = trooper_scp:delete(Trooper, "/link.txt"),
    ok = trooper_scp:delete(Trooper, "/source.txt"),

    %% Test stream open, write, read, close
    StreamFile = "/stream_test.txt",
    {ok, WriteHandler} = trooper_scp:open(Trooper, StreamFile, [write, creat, binary]),
    ok = trooper_scp:write(WriteHandler, <<"chunk1">>),
    ok = trooper_scp:write(WriteHandler, <<"chunk2">>),
    ok = trooper_scp:close(WriteHandler),

    {ok, ReadHandler} = trooper_scp:open(Trooper, StreamFile, [read, binary]),
    {ok, Chunk1} = trooper_scp:read(ReadHandler, 6),
    ?assertEqual(<<"chunk1">>, Chunk1),
    {ok, Chunk2} = trooper_scp:read(ReadHandler, 6),
    ?assertEqual(<<"chunk2">>, Chunk2),
    eof = trooper_scp:read(ReadHandler, 6),
    ok = trooper_scp:close(ReadHandler),
    ok = trooper_scp:delete(Trooper, StreamFile),

    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.

scp_errors_test() ->
    {ok, Sshd, Port} = start_daemon(),
    Opts = [{host, "localhost"},
            {port, Port},
            {user, ?USERNAME},
            {id_rsa, {file, ?BASE_PATH "/user/id_rsa"}}],
    {ok, Trooper} = trooper_ssh:start(Opts),

    {error, _} = trooper_scp:read_file(Trooper, "/non_existent_file.txt"),
    {error, _} = trooper_scp:open(Trooper, "/invalid_dir/file.txt", [read]),

    ok = trooper_ssh:stop(Trooper),
    ok = stop_daemon(Sshd),
    ok.
