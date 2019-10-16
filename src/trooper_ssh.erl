%% @doc Trooper SSH is in charge of connect to the remote hosts using the SSH
%%      application. It lets to tropper to execute commands on those remote
%%      servers and returns the output.
%%
%%      You have two ways to run commands: simple and long polling.
%%
%%      Simple means you send a command (i.e. `uname -r') and you get a sync
%%      response to your request.
%%
%%      <pre lang="erlang"><![CDATA[
%%      trooper_ssh:exec(Trooper, "uname -r").
%%      % {ok, 0, <<"3.16.0-0.bpo.4-amd64">>}
%%      ]]></pre>
%%
%%      Long polling means you send a command (i.e. ping -c 15 127.0.0.1)
%%      and your process starts to receive all of the output of that remote
%%      process:
%%
%%      <pre lang="erlang"><![CDATA[
%%      trooper_ssh:exec_long_polling(Trooper, "ping -c 15 127.0.0.1").
%%      % ok
%%      flush().
%%      % {continue, <<"PING 127.0.0.1 (127.0.0.1) 56(84) "...>>}
%%      ]]></pre>
%%
%%      Until you receive the `closed' event.
%% @end
-module(trooper_ssh).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-define(CONNECT_TIMEOUT, 60000).
-define(CHANNEL_TIMEOUT, 60000).
-define(COMMAND_TIMEOUT, 60000).

-export([
    start/1,
    start_link/1,
    start_chan/1,
    stop_chan/1,
    transaction/2,
    get_pid/1,
    stop/1,
    exec/2,
    exec/3,
    exec_long_polling/2,
    exec_long_polling/3
]).

-record(trooper_ssh, {
    pid :: pid(),
    opts :: proplists:proplist()
}).

-record(trooper_ssh_chan, {
    trooper_ssh :: trooper_ssh(),
    channel :: integer()
}).

-opaque trooper_ssh() :: #trooper_ssh{}.
-opaque trooper_ssh_chan() :: #trooper_ssh_chan{}.

-type opts() :: [opt()].
-type opt() :: {opt_key(), opt_value()}.
-type opt_value() :: term().
-type opt_key() :: atom().

-export_type([trooper_ssh/0,
              trooper_ssh_chan/0,
              opts/0]).

-spec start(opts()) -> {ok, trooper_ssh()} | {error, reason()}.
%% @doc Starts the SSH connection given the parameters.
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
        add_opt(dsa_pass_phrase, Opts),
    ConnOpts = case have_certificate(OtherOpts) of
        true -> [{key_cb, {trooper_keys, OtherOpts ++ Options}}|Options];
        false -> Options
    end,
    case ssh:connect(Host, Port, ConnOpts, ?CONNECT_TIMEOUT) of
        {ok, PID} when is_pid(PID) ->
            {ok, #trooper_ssh{
                pid = PID,
                opts = ConnOpts ++ OtherOpts
            }};
        {error, Reason} ->
            {error, Reason}
    end.


-spec start_chan(trooper_ssh()) -> {ok, trooper_ssh_chan()}.
%% @doc generates SSH channel to run the commands in the same context.
start_chan(#trooper_ssh{pid = PID} = TrooperSsh) ->
    {ok, Chan} = ssh_connection:session_channel(PID, ?CHANNEL_TIMEOUT),
    {ok, #trooper_ssh_chan{channel = Chan, trooper_ssh = TrooperSsh}}.


-spec stop_chan(trooper_ssh_chan()) -> ok.
%% @doc stops a ssh channel.
stop_chan(#trooper_ssh_chan{trooper_ssh = #trooper_ssh{pid = Conn},
                            channel = Chan}) ->
    ssh_connection:close(Conn, Chan).


-spec transaction(opts(), fun((trooper_ssh()) -> any())) -> any().
%% @doc Starts and stops a SSH connection to be available only for the
%%      closure passed as second parameter.
transaction(Opts, Fun) ->
    case start(Opts) of
        {ok, Trooper} ->
            try
                Fun(Trooper)
            after
                stop(Trooper)
            end;
        Error ->
            Error
    end.


-spec start_link(opts()) -> {ok, trooper_ssh()} | {error, reason()}.
%% @doc Starts the SSH connection given the parameters.
start_link(Opts) ->
    case start(Opts) of
        {ok, Trooper} ->
            link(get_pid(Trooper)),
            {ok, Trooper};
        Error ->
            Error
    end.


-spec get_pid(trooper_ssh()) -> pid().
%% @doc Retrieves the PID from a trooper_ssh type data.
get_pid(#trooper_ssh{pid = PID}) ->
    PID.


-spec have_certificate(opts()) -> boolean().
%% @doc Check if it's needed to use trooper_keys or not.
%% @private
have_certificate(Options) ->
    CertOpts = [rsa_pass_phrase, dsa_pass_phrase,
                id_ecdsa, id_rsa, id_dsa],
    lists:any(fun({Key, _}) -> lists:member(Key, CertOpts) end, Options).


-spec add_opt(opt_key(), opts()) -> opts().
%% @doc Get the option if exists in the second param or an empty list otherwise.
%% @private
add_opt(Name, Opts) ->
    case proplists:get_value(Name, Opts, undefined) of
        undefined -> [];
        Value -> [{Name, Value}]
    end.


-spec stop(trooper_ssh()) -> ok.
%% @doc Stops the SSH connection.
stop(#trooper_ssh{pid = Conn}) ->
    ok = ssh:close(Conn).


-spec exec_long_polling(trooper_ssh() | trooper_ssh_chan(),
                        CommandFormat :: string(),
                        Args :: [term()]) -> pid().
%% @doc Executes the command in background setting the current process as the
%%      receiver for the incoming information from the SSH connection.
%%      This function let us to use the format and args way to create the
%%      command to be execute in the remote server.
%% @end
exec_long_polling(TrooperSSH, CommandFormat, Args) ->
    Command = io_lib:format(CommandFormat, Args),
    exec_long_polling(TrooperSSH, Command).


-type exit_status() :: integer().
-type reason() :: atom() | string().

-spec exec(trooper_ssh() | trooper_ssh_chan(),
           CommandFormat :: string(), Args :: [term()]) ->
      {ok, exit_status(), binary()} | {error, reason()}.
%% @doc Executes the command in background setting the current process as the
%%      receiver for the incoming information from the SSH connection.
%%      This function let us to use the format and args way to create the
%%      command to be execute in the remote server.
%% @end
exec(TrooperSSH, CommandFormat, Args) ->
    Command = io_lib:format(CommandFormat, Args),
    exec(TrooperSSH, Command).


-spec exec_long_polling(trooper_ssh() | trooper_ssh_chan(),
                        Command :: string()) -> pid().
%% @doc Executes the command in background setting the current process as the
%%      receiver for the incoming information from the SSH connection.
%% @end
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


-spec exec(trooper_ssh() | trooper_ssh_chan(), Command :: string()) ->
      {ok, exit_status(), binary()} | {error, reason()}.
%% @doc Executes the command in background setting the current process as the
%%      receiver for the incoming information from the SSH connection.
%% @end
exec(#trooper_ssh{} = TrooperSsh, Command) ->
    {ok, Chan} = start_chan(TrooperSsh),
    Result = exec(Chan, Command),
    stop_chan(Chan),
    Result;

exec(#trooper_ssh_chan{trooper_ssh = #trooper_ssh{pid = Conn},
                       channel = Chan}, Command) ->
    case ssh_connection:exec(Conn, Chan, Command, ?COMMAND_TIMEOUT) of
        success ->
            get_all_info(Chan, <<>>, 0);
        Error ->
            Error
    end.


-spec get_and_send_all_info(pid(),
                            ssh:ssh_connection_ref(),
                            ssh:ssh_channel_id()) -> ok.
%% @doc Loop for exec_long_polling functions. Receives all the information from
%%      the SSH connection and send back to the PID in a simpler format.
%% @private
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
            PID ! closed,
            ok;
        stop ->
            PID ! stopped,
            ok = ssh_connection:close(Conn, Chan);
        _DroppingMsg ->
            get_and_send_all_info(PID, Conn, Chan)
    after ?COMMAND_TIMEOUT ->
        PID ! {error, etimeout},
        ok
    end.


-spec get_all_info(ssh:ssh_channel_id(), binary(), exit_status()) ->
      {ok, exit_status(), binary()} |
      {error, {incomplete, binary()}} |
      {error, etimeout}.
%% @doc Loop for exec commands. It's on charge of receive all of the information
%%      from the SSH connection until it's closed and send back as return of
%%      the function.
%% @private
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
