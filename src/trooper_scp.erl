%% @doc Trooper SCP is in charge of handle remote/local files copy.
%%
%%      You need to have a trooper SSH connection opened to list
%%      remote files, upload local files to the remote place or
%%      download remote files.
%% @end
-module(trooper_scp).
-author('manuel@altenwald.com').

-export([
    list_dir/2,
    make_dir/2,
    del_dir/2,
    rename/3,
    delete/2,
    make_symlink/3,
    read_file/2,
    write_file/3,

    open/3,
    close/1,
    read/2,
    write/2
]).

-record(file_handler, {
    channel :: pid(),
    handler :: term()
}).

-export_type([file_handler/0]).

-opaque file_handler() :: #file_handler{}.

-type reason() :: term().

channel(Trooper, Run) ->
    case ssh_sftp:start_channel(trooper_ssh:get_pid(Trooper)) of
        {ok, PID} ->
            try
                Run(PID)
            after
                ssh_sftp:stop_channel(PID)
            end;
        {error, _} = Error ->
            Error
    end.

-spec list_dir(trooper_ssh:trooper_ssh(), Path :: string()) ->
      {ok, [string()]} | {error, reason()}.
%%@doc List remote directory.
list_dir(Trooper, Path) ->
    channel(Trooper, fun(PID) -> ssh_sftp:list_dir(PID, Path) end).

-spec make_dir(trooper_ssh:trooper_ssh(), Name :: string()) ->
      ok | {error, reason()}.
%%@doc Creates a remote directory.
make_dir(Trooper, Name) ->
    channel(Trooper, fun(PID) -> ssh_sftp:make_dir(PID, Name) end).

-spec del_dir(trooper_ssh:trooper_ssh(), Name :: string()) ->
      ok | {error, reason()}.
%%@doc Removes a remote directory.
del_dir(Trooper, Name) ->
    channel(Trooper, fun(PID) -> ssh_sftp:del_dir(PID, Name) end).

-spec rename(trooper_ssh:trooper_ssh(), OldName :: string(), NewName :: string()) ->
      ok | {error, reason()}.
%%@doc Renames a remote file.
rename(Trooper, OldName, NewName) ->
    channel(Trooper, fun(PID) -> ssh_sftp:rename(PID, OldName, NewName) end).

-spec delete(trooper_ssh:trooper_ssh(), Name :: string()) ->
      ok | {error, reason()}.
%%@doc Deletes a remote file.
delete(Trooper, Name) ->
    channel(Trooper, fun(PID) -> ssh_sftp:delete(PID, Name) end).

-spec make_symlink(trooper_ssh:trooper_ssh(), Name :: string(), Target :: string()) ->
      ok | {error, reason()}.
%%@doc Makes a symlink in the a remote server.
make_symlink(Trooper, Name, Target) ->
    channel(Trooper, fun(PID) -> ssh_sftp:make_symlink(PID, Name, Target) end).

-spec read_file(trooper_ssh:trooper_ssh(), Name :: string()) ->
      {ok, binary()} | {error, reason()}.
%%@doc Reads a remote file content.
read_file(Trooper, Name) ->
    channel(Trooper, fun(PID) -> ssh_sftp:read_file(PID, Name) end).

-spec write_file(trooper_ssh:trooper_ssh(), Name :: string(), Content :: iolist()) ->
      ok | {error, reason()}.
%%@doc Writes a remote file content.
write_file(Trooper, Name, Content) ->
    channel(Trooper, fun(PID) -> ssh_sftp:write_file(PID, Name, Content) end).

-type mode() :: [read | write | append | binary | raw].

-spec open(trooper_ssh:trooper_ssh(), Name :: string(), Mode :: mode()) ->
      {ok, file_handler()} | {error, reason()}.
%%@doc Opens a remote file using a handler to let use read and write.
%%     Depending on the mode in use.
open(Trooper, Name, Mode) ->
    case ssh_sftp:start_channel(trooper_ssh:get_pid(Trooper)) of
        {ok, ChannelPid} ->
            case ssh_sftp:open(ChannelPid, Name, Mode) of
                {ok, Handler} ->
                    {ok, #file_handler{channel = ChannelPid, handler = Handler}};
                {error, _} = Error ->
                    ssh_sftp:stop_channel(ChannelPid),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec close(file_handler()) -> ok | {error, reason()}.
%%@doc Closes the handler.
close(#file_handler{channel = ChannelPid, handler = Handler}) ->
    Res = ssh_sftp:close(ChannelPid, Handler),
    ssh_sftp:stop_channel(ChannelPid),
    Res.

-spec read(file_handler(), Len :: pos_integer()) ->
      {ok, binary()} | eof | {error, reason()}.
%%@doc Reads information from an opened remote file.
read(#file_handler{channel = ChannelPid, handler = Handler}, Len) ->
    ssh_sftp:read(ChannelPid, Handler, Len).

-spec write(file_handler(), Data :: iolist()) ->
      ok | {error, reason()}.
%%@doc Writes information to an opened remote file.
write(#file_handler{channel = ChannelPid, handler = Handler}, Data) ->
    ssh_sftp:write(ChannelPid, Handler, Data).
