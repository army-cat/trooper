

# Module trooper_proxy #
* [Description](#description)
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
command (i.e. `nc server2 22`).

The second connection is established against the random port and local
interface. This way all of the information is sent to the remote server
to that proxy command and therefore to the new server.

At the end, you are connected to the last server in the list, but thru
all of the previous servers. Note that you have to provide the way to
connect to the following server keeping in mind you're in the previous
one.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-1">start/1</a></td><td>starts the proxy connections to the remote servers.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Configs::[<a href="trooper_ssh.md#type-opts">trooper_ssh:opts()</a>]) -&gt; {ok, <a href="trooper_ssh.md#type-trooper_ssh">trooper_ssh:trooper_ssh()</a>}
</code></pre>
<br />

starts the proxy connections to the remote servers.

