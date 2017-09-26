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
%%      command (i.e. `nc server2 22').
%%
%%      The second connection is established against the random port and local
%%      interface. This way all of the information is sent to the remote server
%%      to that proxy command and therefore to the new server.
%%
%%      At the end, you are connected to the last server in the list, but thru
%%      all of the previous servers. Note that you have to provide the way to
%%      connect to the following server keeping in mind you're in the previous
%%      one.
%% @end
-module(trooper_proxy).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([start/1]).

-define(COMMAND_TIMEOUT, 60000).
-define(LISTENING_TIMEOUT, 1000).
-define(LOCAL_IP, "127.0.0.1").


-spec start([trooper_ssh:opts()]) -> {ok, trooper_ssh:trooper_ssh()}.
%% @doc starts the proxy connections to the remote servers.
start([Config1,Config2|Configs]) ->
    Cmd = proplists:get_value(proxy, Config1, "nc ~s ~b"),
    Host = proplists:get_value(host, Config2, undefined),
    Port = proplists:get_value(port, Config2, 22),
    NewConfig = proplists:delete(proxy, Config1),
    {ok, Trooper} = trooper_ssh:start(NewConfig),
    Parent = self(),
    spawn(fun() ->
        PID = trooper_ssh:exec_long_polling(Trooper, Cmd, [Host, Port]),
        {ok, LSocket} = gen_tcp:listen(0, [binary, {active, true}]),
        {ok, LocalPort} = inet:port(LSocket),
        Parent ! {port, LocalPort},
        {ok, Socket} = gen_tcp:accept(LSocket),
        processing(PID, Socket),
        gen_tcp:close(LSocket)
    end),
    LocalPort = receive
        {port, LPort} -> LPort
    after
        ?LISTENING_TIMEOUT -> throw({error, {Host, Port}})
    end,
    C0 = proplists:delete(host, Config2),
    C1 = proplists:delete(port, C0),
    NewConfig2 = [{host, ?LOCAL_IP}, {port, LocalPort}|C1],
    start([NewConfig2|Configs]);

start([Config]) ->
    {ok, _Trooper} = trooper_ssh:start(Config).


-spec processing(pid(), gen_tcp:socket()) -> ok.
%% @doc this function is in charge of handling the proxy information. Everything
%%      coming from SSH is sent to the TCP connection and everything from the
%%      TCP connection is sent back to the SSH connection.
%% @private
processing(PID, Socket) ->
    receive
        {tcp, Socket, Data} ->
            PID ! {send, Data},
            processing(PID, Socket);
        {tcp_closed, Socket} ->
            PID ! stop,
            ok = gen_tcp:close(Socket);
        {continue, Data} ->
            gen_tcp:send(Socket, Data),
            processing(PID, Socket);
        {exit_status, _} ->
            processing(PID, Socket);
        _Error ->
            ok = gen_tcp:close(Socket)
    after
        ?COMMAND_TIMEOUT ->
            ok = gen_tcp:close(Socket)
    end.
