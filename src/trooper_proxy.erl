-module(trooper_proxy).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([start/1]).

-define(COMMAND_TIMEOUT, 60000).
-define(LISTENING_TIMEOUT, 1000).
-define(LOCAL_IP, "127.0.0.1").

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


processing(PID, Socket) ->
    receive
        {tcp, Socket, Data} ->
            PID ! {send, Data},
            processing(PID, Socket);
        {tcp_closed, Socket} ->
            PID ! stop,
            gen_tcp:close(Socket);
        {continue, Data} ->
            gen_tcp:send(Socket, Data),
            processing(PID, Socket);
        {exit_status, _} ->
            processing(PID, Socket);
        _Error ->
            gen_tcp:close(Socket)
    after
        ?COMMAND_TIMEOUT ->
            gen_tcp:close(Socket)
    end.
