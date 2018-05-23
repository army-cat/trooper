

# Module trooper_ssh #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Trooper SSH is in charge of connect to the remote hosts using the SSH
application.

<a name="description"></a>

## Description ##

It lets to tropper to execute commands on those remote
servers and returns the output.

You have two ways to run commands: simple and long polling.

Simple means you send a command (i.e. `uname -r`) and you get a sync
response to your request.

```erlang

       trooper_ssh:exec(Trooper, "uname -r").
       % {ok, 0, <<"3.16.0-0.bpo.4-amd64">>}
```

Long polling means you send a command (i.e. ping -c 15 127.0.0.1)
and your process starts to receive all of the output of that remote
process:

```erlang

       trooper_ssh:exec_long_polling(Trooper, "ping -c 15 127.0.0.1").
       % ok
       flush().
       % {continue, <<"PING 127.0.0.1 (127.0.0.1) 56(84) "...>>}
```

Until you receive the `closed` event.
<a name="types"></a>

## Data Types ##




### <a name="type-exit_status">exit_status()</a> ###


<pre><code>
exit_status() = integer()
</code></pre>




### <a name="type-opt">opt()</a> ###


<pre><code>
opt() = {<a href="#type-opt_key">opt_key()</a>, <a href="#type-opt_value">opt_value()</a>}
</code></pre>




### <a name="type-opt_key">opt_key()</a> ###


<pre><code>
opt_key() = atom()
</code></pre>




### <a name="type-opt_value">opt_value()</a> ###


<pre><code>
opt_value() = term()
</code></pre>




### <a name="type-opts">opts()</a> ###


<pre><code>
opts() = [<a href="#type-opt">opt()</a>]
</code></pre>




### <a name="type-reason">reason()</a> ###


<pre><code>
reason() = atom() | string()
</code></pre>




### <a name="type-trooper_ssh">trooper_ssh()</a> ###


__abstract datatype__: `trooper_ssh()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exec-2">exec/2</a></td><td>Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.</td></tr><tr><td valign="top"><a href="#exec-3">exec/3</a></td><td>Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.</td></tr><tr><td valign="top"><a href="#exec_long_polling-2">exec_long_polling/2</a></td><td>Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.</td></tr><tr><td valign="top"><a href="#exec_long_polling-3">exec_long_polling/3</a></td><td>Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.</td></tr><tr><td valign="top"><a href="#get_pid-1">get_pid/1</a></td><td>Retrieves the PID from a trooper_ssh type data.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Starts the SSH connection given the parameters.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Starts the SSH connection given the parameters.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stops the SSH connection.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="exec-2"></a>

### exec/2 ###

<pre><code>
exec(Trooper_ssh::<a href="#type-trooper_ssh">trooper_ssh()</a>, Command::string()) -&gt; {ok, <a href="#type-exit_status">exit_status()</a>, binary()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.

<a name="exec-3"></a>

### exec/3 ###

<pre><code>
exec(TrooperSSH::<a href="#type-trooper_ssh">trooper_ssh()</a>, CommandFormat::string(), Args::[term()]) -&gt; {ok, <a href="#type-exit_status">exit_status()</a>, binary()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.
This function let us to use the format and args way to create the
command to be execute in the remote server.

<a name="exec_long_polling-2"></a>

### exec_long_polling/2 ###

<pre><code>
exec_long_polling(Trooper_ssh::<a href="#type-trooper_ssh">trooper_ssh()</a>, Command::string()) -&gt; pid()
</code></pre>
<br />

Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.

<a name="exec_long_polling-3"></a>

### exec_long_polling/3 ###

<pre><code>
exec_long_polling(TrooperSSH::<a href="#type-trooper_ssh">trooper_ssh()</a>, CommandFormat::string(), Args::[term()]) -&gt; pid()
</code></pre>
<br />

Executes the command in background setting the current process as the
receiver for the incoming information from the SSH connection.
This function let us to use the format and args way to create the
command to be execute in the remote server.

<a name="get_pid-1"></a>

### get_pid/1 ###

<pre><code>
get_pid(Trooper_ssh::<a href="#type-trooper_ssh">trooper_ssh()</a>) -&gt; pid()
</code></pre>
<br />

Retrieves the PID from a trooper_ssh type data.

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Opts::<a href="#type-opts">opts()</a>) -&gt; {ok, <a href="#type-trooper_ssh">trooper_ssh()</a>} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Starts the SSH connection given the parameters.

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Opts::<a href="#type-opts">opts()</a>) -&gt; {ok, <a href="#type-trooper_ssh">trooper_ssh()</a>} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Starts the SSH connection given the parameters.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Trooper_ssh::<a href="#type-trooper_ssh">trooper_ssh()</a>) -&gt; ok
</code></pre>
<br />

Stops the SSH connection.

