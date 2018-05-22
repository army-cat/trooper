-module(trooper_proxy_chain).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(supervisor_bridge).

-define(COMMAND_TIMEOUT, 60000).

-export([start_link/2, start_link/5, init/1, terminate/2]).


start_link(From, Config) ->
    supervisor_bridge:start_link(?MODULE, [From, Config]).


start_link(From, Config, Cmd, Host, Port) ->
    supervisor_bridge:start_link(?MODULE, [From, Config, Cmd, Host, Port]).


init([From, Config]) ->
    {ok, Trooper} = trooper_ssh:start(Config),
    From ! {trooper, Trooper},
    {ok, trooper_ssh:get_pid(Trooper), undefined};

init([From, Config, Cmd, Host, Port]) ->
    {ok, Trooper} = trooper_ssh:start_link(Config),
    {ok, spawn_link(fun() ->
        PID = trooper_ssh:exec_long_polling(Trooper, Cmd, [Host, Port]),
        {ok, LSocket} = gen_tcp:listen(0, [binary, {active, true}]),
        {ok, LocalPort} = inet:port(LSocket),
        From ! {port, LocalPort},
        {ok, Socket} = gen_tcp:accept(LSocket),
        processing(PID, Socket),
        gen_tcp:close(LSocket)
    end), Trooper}.


terminate(_Reason, Trooper) ->
    trooper_ssh:stop(Trooper).


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
            PID ! stop,
            ok = gen_tcp:close(Socket)
    after
        ?COMMAND_TIMEOUT ->
            PID ! stop,
            ok = gen_tcp:close(Socket)
    end.
