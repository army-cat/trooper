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
    exec/3
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

exec(TrooperSSH, CommandFormat, Args) ->
    Command = io_lib:format(CommandFormat, Args),
    exec(TrooperSSH, Command).

exec(#trooper_ssh{pid=Conn}, Command) ->
    {ok, Chan} = ssh_connection:session_channel(Conn, ?CHANNEL_TIMEOUT),
    clean_output(),
    case ssh_connection:exec(Conn, Chan, Command, ?COMMAND_TIMEOUT) of
        success ->
            get_all_info(Chan, <<>>, 0);
        Error ->
            Error
    end.

clean_output() ->
    receive
        {ssh_cm, _PID, _Data} ->
            clean_output()
    after 0 ->
        ok
    end.

get_all_info(Chan, Received, ExitStatus) ->
    receive
        {ssh_cm, _PID, Data} ->
            case handle_msg(Chan, Data) of
                {continue, Chunk} ->
                    NewReceived = <<Received/binary, Chunk/binary>>,
                    get_all_info(Chan, NewReceived, ExitStatus);
                {exit_status, NewExitStatus} ->
                    get_all_info(Chan, Received, NewExitStatus);
                returns ->
                    {ok, ExitStatus, Received}
            end
    after ?COMMAND_TIMEOUT ->
        case Received of
            <<>> -> {error, etimeout};
            _ -> {error, {incomplete, Received}}
        end
    end.

handle_msg(Chan, {data, Chan, _Type, Data}) ->
    {continue, Data};

handle_msg(Chan, {exit_status, Chan, ExitStatus}) ->
    {exit_status, ExitStatus};

handle_msg(Chan, {eof, Chan}) ->
    {continue, <<>>};

handle_msg(Chan, {closed, Chan}) ->
    returns.
