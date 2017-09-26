-module(trooper_ssh).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-define(CONNECT_TIMEOUT, 60000).
-define(CHANNEL_TIMEOUT, 60000).
-define(COMMAND_TIMEOUT, 60000).

-export([
    start/1,
    stop/1,
    exec/2,
    exec/3,
    exec_long_polling/2,
    exec_long_polling/3
]).

-include("trooper.hrl").

start(Opts) ->
    Host = proplists:get_value(host, Opts, undefined),
    Port = proplists:get_value(port, Opts, 22),
    OtherOpts =
        add_opt(id_rsa, Opts) ++
        add_opt(id_dsa, Opts) ++
        add_opt(id_ecdsa, Opts),
    Options = [
        {connect_timeout, ?CONNECT_TIMEOUT},
        {user, proplists:get_value(user, Opts, undefined)},
        {quiet_mode, true},
        {silently_accept_hosts, true},
        {user_interaction, false}
    ] ++
        add_opt(password, Opts) ++
        add_opt(rsa_pass_phrase, Opts) ++
        add_opt(dsa_pass_phrase, Opts) ++
        add_opt(ecdsa_pass_phrase, Opts),
    ConnOpts = [{key_cb, {trooper_keys, OtherOpts ++ Options}}|Options],
    case ssh:connect(Host, Port, ConnOpts, ?CONNECT_TIMEOUT) of
        {ok, PID} when is_pid(PID) ->
            {ok, #trooper_ssh{
                pid = PID,
                opts = ConnOpts ++ OtherOpts
            }};
        {error, Reason} ->
            {error, Reason}
    end.

add_opt(Name, Opts) ->
    case proplists:get_value(Name, Opts, undefined) of
        undefined -> [];
        Value -> [{Name, Value}]
    end.

stop(#trooper_ssh{pid=Conn}) ->
    ssh:close(Conn).

exec_long_polling(TrooperSSH, CommandFormat, Args) ->
    Command = io_lib:format(CommandFormat, Args),
    exec_long_polling(TrooperSSH, Command).

exec(TrooperSSH, CommandFormat, Args) ->
    Command = io_lib:format(CommandFormat, Args),
    exec(TrooperSSH, Command).

exec_long_polling(#trooper_ssh{pid=Conn}, Command) ->
    Parent = self(),
    spawn_link(fun() ->
        {ok, Chan} = ssh_connection:session_channel(Conn, ?CHANNEL_TIMEOUT),
        case ssh_connection:exec(Conn, Chan, Command, ?COMMAND_TIMEOUT) of
            success ->
                get_and_send_all_info(Parent, Conn, Chan);
            Error ->
                Error
        end
    end).

exec(#trooper_ssh{pid=Conn}, Command) ->
    {ok, Chan} = ssh_connection:session_channel(Conn, ?CHANNEL_TIMEOUT),
    case ssh_connection:exec(Conn, Chan, Command, ?COMMAND_TIMEOUT) of
        success ->
            get_all_info(Chan, <<>>, 0);
        Error ->
            Error
    end.

get_and_send_all_info(PID, Conn, Chan) ->
    receive
        {send, Data} ->
            ssh_connection:send(Conn, Chan, Data),
            get_and_send_all_info(PID, Conn, Chan);
        {ssh_cm, _PID, {data, Chan, _Type, Data}} ->
            PID ! {continue, Data},
            get_and_send_all_info(PID, Conn, Chan);
        {ssh_cm, _PID, {eof, Chan}} ->
            get_and_send_all_info(PID, Conn, Chan);
        {ssh_cm, _PID, {exit_status, Chan, ExitStatus}} ->
            PID ! {exit_status, ExitStatus},
            get_and_send_all_info(PID, Conn, Chan);
        {ssh_cm, _PID, {closed, Chan}} ->
            PID ! closed;
        stop ->
            PID ! stopped,
            ssh_connection:close(Conn, Chan);
        _DroppingMsg ->
            get_and_send_all_info(PID, Conn, Chan)
    after ?COMMAND_TIMEOUT ->
        PID ! {error, etimeout}
    end.

get_all_info(Chan, Received, ExitStatus) ->
    receive
        {ssh_cm, _PID, {data, Chan, _Type, Chunk}} ->
            NewReceived = <<Received/binary, Chunk/binary>>,
            get_all_info(Chan, NewReceived, ExitStatus);
        {ssh_cm, _PID, {eof, Chan}} ->
            get_all_info(Chan, Received, ExitStatus);
        {ssh_cm, _PID, {exit_status, Chan, NewExitStatus}} ->
            get_all_info(Chan, Received, NewExitStatus);
        {ssh_cm, _PID, {closed, Chan}} ->
            {ok, ExitStatus, Received};
        _DroppingMsg ->
            get_all_info(Chan, Received, ExitStatus)
    after ?COMMAND_TIMEOUT ->
        case Received of
            <<>> -> {error, etimeout};
            _ -> {error, {incomplete, Received}}
        end
    end.
