%% @doc Trooper SCP is in charge of handle remote/local files copy.
%%
%%      You need to have a tropper SSH connection opened to list
%%      remote files, upload local files to the remote place or
%%      download remote files.
%% @end
-module(trooper_scp).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

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
    trooper :: trooper:trooper(),
    handler :: term()
}).

-export_type([file_handler/0]).

-opaque file_handler() :: #file_handler{}.

-type reason() :: atom().

channel(Trooper, Run) ->
    case ssh_sftp:start_channel(trooper_ssh:get_pid(Trooper)) of
        {ok, PID} ->
            Result = Run(PID),
            ssh_sftp:stop_channel(PID),
            Result;
        {error, _} = Error ->
            Error
    end.

-spec list_dir(trooper_ssh:trooper(), Path :: string()) ->
      {ok, [string()]} | {error, reason()}.
%%@doc List remote directory.
list_dir(Trooper, Path) ->
    channel(Trooper, fun(PID) -> ssh_sftp:list_dir(PID, Path) end).

-spec make_dir(trooper_ssh:trooper(), Name :: string()) ->
      ok | {error, reason()}.
%%@doc Creates a remote directory.
make_dir(Trooper, Name) ->
    channel(Trooper, fun(PID) -> ssh_sftp:make_dir(PID, Name) end).

-spec del_dir(trooper_ssh:trooper(), Name :: string()) ->
      ok | {error, reason()}.
%%@doc Removes a remote directory.
del_dir(Trooper, Name) ->
    channel(Trooper, fun(PID) -> ssh_sftp:del_dir(PID, Name) end).

-spec rename(trooper_ssh:trooper(), OldName :: string(), NewName :: string()) ->
      ok | {error, reason()}.
%%@doc Renames a remote file.
rename(Trooper, OldName, NewName) ->
    channel(Trooper, fun(PID) -> ssh_sftp:rename(PID, OldName, NewName) end).

-spec delete(trooper_ssh:trooper(), Name :: string()) ->
      ok | {error, reason()}.
%%@doc Deletes a remote file.
delete(Trooper, Name) ->
    channel(Trooper, fun(PID) -> ssh_sftp:delete(PID, Name) end).

-spec make_symlink(trooper_ssh:trooper(), Name :: string(), Target :: string()) ->
      ok | {error, reason()}.
%%@doc Makes a symlink in the a remote server.
make_symlink(Trooper, Name, Target) ->
    channel(Trooper, fun(PID) -> ssh_sftp:make_symlink(PID, Name, Target) end).

-spec read_file(trooper_ssh:trooper(), Name :: string()) ->
      {ok, binary()} | {error, reason()}.
%%@doc Reads a remote file content.
read_file(Trooper, Name) ->
    channel(Trooper, fun(PID) -> ssh_sftp:read_file(PID, Name) end).

-spec write_file(trooper_ssh:trooper(), Name :: string(), Content :: iolist()) ->
      ok | {error, reason()}.
%%@doc Writes a remote file content.
write_file(Trooper, Name, Content) ->
    channel(Trooper, fun(PID) -> ssh_sftp:write_file(PID, Name, Content) end).

-spec open(file_handler(), Name :: string(), Mode :: ssh_sftp:mode()) ->
      {ok, file_handler()} | {error, reason()}.
%%@doc Opens a remote file using a handler to let use read and write.
%%     Depending on the mode in use.
open(#file_handler{trooper = Trooper, handler = Handler}, Name, Mode) ->
    channel(Trooper, fun(PID) -> ssh_sftp:open(PID, Handler, Name, Mode) end).

-spec close(file_handler()) -> ok | {error, reason()}.
%%@doc Closes the handler.
close(#file_handler{trooper = Trooper, handler = Handler}) ->
    channel(Trooper, fun(PID) -> ssh_sftp:close(PID, Handler) end).

-spec read(file_handler(), Len :: pos_integer()) ->
      {ok, binary()} | eof | {ok, reason()}.
%%@doc Reads information from an opened remote file.
read(#file_handler{trooper = Trooper, handler = Handler}, Len) ->
    channel(Trooper, fun(PID) -> ssh_sftp:read(PID, Handler, Len) end).

-spec write(file_handler(), Data :: iolist()) ->
      ok | {error, reason()}.
%%@doc Writes information to an opened remote file.
write(#file_handler{trooper = Trooper, handler = Handler}, Data) ->
    channel(Trooper, fun(PID) -> ssh_sftp:write(PID, Handler, Data) end).
