

# Module trooper_proxy #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Trooper proxy generates a chain of connections to arrive to the desired
remote server.

<a name="description"></a>

## Description ##

Given a list of connections:

```erlang

       [[{host, "server1"}, {user, "user1"}, {password, "pass1"}],
        [{host, "server2"}, {user, "user2"}, {password, "pass2"}]]
```

The proxy creates the connection to the first server and a server
listening in a random port and running in a long polling way a proxy
command (i.e. `nc server2 22`), you can provide the command through
the options. For example, in the previous example:

```erlang

       [[{host, "server1"}, {user, "user1"}, {password, "pass1"},
         {proxy, "nc ~s ~b"}], ...]
```

Keep in mind it should receive two params, the first one is the name
of the server (~s) where it should to connect and the second one is the
port (~b).

The second connection is established against the random port and local
interface. This way all of the information is sent to the remote server
to that proxy command and therefore to the new server.

At the end, you are connected to the last server in the list, but
through all of the previous servers. Note that you have to provide
the way to connect to the following server keeping in mind you're in
the previous one.
<a name="types"></a>

## Data Types ##




### <a name="type-exit_status">exit_status()</a> ###


<pre><code>
exit_status() = integer()
</code></pre>




### <a name="type-reason">reason()</a> ###


<pre><code>
reason() = atom() | string()
</code></pre>




### <a name="type-trooper_proxy">trooper_proxy()</a> ###


__abstract datatype__: `trooper_proxy()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exec-2">exec/2</a></td><td>Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.</td></tr><tr><td valign="top"><a href="#exec-3">exec/3</a></td><td>Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.</td></tr><tr><td valign="top"><a href="#exec_long_polling-2">exec_long_polling/2</a></td><td>Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.</td></tr><tr><td valign="top"><a href="#exec_long_polling-3">exec_long_polling/3</a></td><td>Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Starts the proxy connections to the remote servers.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stops the SSH proxy connection.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="exec-2"></a>

### exec/2 ###

<pre><code>
exec(Trooper_proxy::<a href="#type-trooper_proxy">trooper_proxy()</a>, Command::string()) -&gt; {ok, <a href="#type-exit_status">exit_status()</a>, binary()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.

<a name="exec-3"></a>

### exec/3 ###

<pre><code>
exec(Trooper_proxy::<a href="#type-trooper_proxy">trooper_proxy()</a>, CommandFormat::string(), Args::[term()]) -&gt; {ok, <a href="#type-exit_status">exit_status()</a>, binary()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.
This function let us to use the format and args way to create the
command to be execute in the remote server.

<a name="exec_long_polling-2"></a>

### exec_long_polling/2 ###

<pre><code>
exec_long_polling(Trooper_proxy::<a href="#type-trooper_proxy">trooper_proxy()</a>, Command::string()) -&gt; pid()
</code></pre>
<br />

Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.

<a name="exec_long_polling-3"></a>

### exec_long_polling/3 ###

<pre><code>
exec_long_polling(Trooper_proxy::<a href="#type-trooper_proxy">trooper_proxy()</a>, CommandFormat::string(), Args::[term()]) -&gt; pid()
</code></pre>
<br />

Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.
This function let us to use the format and args way to create the
command to be execute in the remote server.

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Configs::[<a href="trooper_ssh.md#type-opts">trooper_ssh:opts()</a>]) -&gt; {ok, <a href="#type-trooper_proxy">trooper_proxy()</a>}
</code></pre>
<br />

Starts the proxy connections to the remote servers.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Trooper_proxy::<a href="#type-trooper_proxy">trooper_proxy()</a>) -&gt; ok
</code></pre>
<br />

Stops the SSH proxy connection.

