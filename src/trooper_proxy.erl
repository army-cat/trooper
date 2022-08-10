%% @doc Trooper proxy generates a chain of connections to arrive to the desired
%%      remote server. Given a list of connections:
%%
%%      <pre lang="erlang">
%%      [[{host, "server1"}, {user, "user1"}, {password, "pass1"}],
%%       [{host, "server2"}, {user, "user2"}, {password, "pass2"}]]
%%      </pre>
%%
%%      The proxy creates the connection to the first server and a server
%%      listening in a random port and running in a long polling way a proxy
%%      command (i.e. `nc server2 22'), you can provide the command through
%%      the options. For example, in the previous example:
%%
%%      <pre lang="erlang">
%%      [[{host, "server1"}, {user, "user1"}, {password, "pass1"},
%%        {proxy, "nc ~s ~b"}], ...]
%%      </pre>
%%
%%      Keep in mind it should receive two params, the first one is the name
%%      of the server (~s) where it should to connect and the second one is the
%%      port (~b).
%%
%%      The second connection is established against the random port and local
%%      interface. This way all of the information is sent to the remote server
%%      to that proxy command and therefore to the new server.
%%
%%      At the end, you are connected to the last server in the list, but
%%      through all of the previous servers. Note that you have to provide
%%      the way to connect to the following server keeping in mind you're in
%%      the previous one.
%% @end
-module(trooper_proxy).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    start/1,
    stop/1,
    exec/2,
    exec/3,
    exec_long_polling/2,
    exec_long_polling/3
]).

-define(LISTENING_TIMEOUT, 1000).
-define(LOCAL_IP, "127.0.0.1").

-record(trooper_proxy, {
    supervisor :: pid(),
    supervisor_ref :: reference(),
    trooper_ssh :: trooper_ssh:trooper_ssh() | undefined
}).

-opaque trooper_proxy() :: #trooper_proxy{}.

-export_type([trooper_proxy/0]).

-spec start([trooper_ssh:opts()]) -> {ok, trooper_proxy()}.
%% @doc Starts the proxy connections to the remote servers.
start(Configs) ->
    Ref = make_ref(),
    {ok, PID} = trooper_proxy_sup:start(Ref),
    start(Configs, #trooper_proxy{supervisor = PID,
                                  supervisor_ref = Ref}).


-spec start([trooper_ssh:opts()], [pid()] | trooper_proxy()) -> {ok, trooper_proxy()}.
%% @private
start([Config1,Config2|Configs], #trooper_proxy{supervisor = Sup} = TProxy) ->
    Cmd = proplists:get_value(proxy, Config1, "nc -w 60 ~s ~b"),
    Host = proplists:get_value(host, Config2, undefined),
    Port = proplists:get_value(port, Config2, 22),
    NewConfig = proplists:delete(proxy, Config1),
    From = self(),
    trooper_proxy_sup:start_child(Sup, [From, NewConfig, Cmd, Host, Port]),
    LocalPort = receive
        {port, LPort} -> LPort
    after
        ?LISTENING_TIMEOUT -> throw({error, {Host, Port}})
    end,
    C0 = proplists:delete(host, Config2),
    C1 = proplists:delete(port, C0),
    NewConfig2 = [{host, ?LOCAL_IP}, {port, LocalPort}|C1],
    start([NewConfig2|Configs], TProxy);

start([Config], #trooper_proxy{supervisor = Sup} = TProxy) ->
    trooper_proxy_sup:start_child(Sup, [self(), Config]),
    Trooper = receive
        {trooper, T} -> T
    after
        ?LISTENING_TIMEOUT -> throw({error, Config})
    end,
    {ok, TProxy#trooper_proxy{trooper_ssh = Trooper}}.


-spec stop(trooper_proxy()) -> ok.
%% @doc Stops the SSH proxy connection.
stop(#trooper_proxy{supervisor_ref = Ref}) ->
    supervisor:terminate_child(trooper_app, Ref).


-spec exec_long_polling(trooper_proxy(), CommandFormat :: string(),
                        Args :: [term()]) -> pid().
%% @doc Executes the command in background setting the current process as the
%%      receiver for the incoming information from the SSH connection.
%%      This function let us to use the format and args way to create the
%%      command to be execute in the remote server.
%% @end
exec_long_polling(#trooper_proxy{trooper_ssh = TrooperSSH}, Cmd, Args) ->
    trooper_ssh:exec_long_polling(TrooperSSH, Cmd, Args).


-type exit_status() :: integer().
-type reason() :: atom() | string().

-spec exec(trooper_proxy(), CommandFormat :: string(), Args :: [term()]) ->
      {ok, exit_status(), binary()} | {error, reason()}.
%% @doc Executes the command in background setting the current process as the
%%      receiver for the incoming information from the SSH connection.
%%      This function let us to use the format and args way to create the
%%      command to be execute in the remote server.
%% @end
exec(#trooper_proxy{trooper_ssh = TrooperSSH}, CommandFormat, Args) ->
    trooper_ssh:exec(TrooperSSH, CommandFormat, Args).


-spec exec_long_polling(trooper_proxy(), Command :: string()) -> pid().
%% @doc Executes the command in background setting the current process as the
%%      receiver for the incoming information from the SSH connection.
%% @end
exec_long_polling(#trooper_proxy{trooper_ssh = TrooperSSH}, Command) ->
    trooper_ssh:exec_long_polling(TrooperSSH, Command).


-spec exec(trooper_proxy(), Command :: string()) ->
      {ok, exit_status(), binary()} | {error, reason()}.
%% @doc Executes the command in background setting the current process as the
%%      receiver for the incoming information from the SSH connection.
%% @end
exec(#trooper_proxy{trooper_ssh = TrooperSSH}, Command) ->
    trooper_ssh:exec(TrooperSSH, Command).
