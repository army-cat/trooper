

# Trooper #

Copyright (c) 2016 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![License: LGPL 2.1](https://img.shields.io/github/license/altenwald/trooper.svg)](https://raw.githubusercontent.com/altenwald/trooper/master/COPYING)

Trooper is a soldier in charge to go to other machines (via SSH) and follow your commands.


### <a name="Requirements">Requirements</a> ###

Trooper requires to be run over an Erlang/OTP 17+.


### <a name="Example">Example</a> ###

```erlang
1> {ok, File} = file:read_file("/home/trooper/.ssh/id_rsa").
{ok,<<"-----BEGIN RSA PRIVATE KEY-----\nMIIE"...>>}
2> {ok,Trooper} = trooper_ssh:start([{host, "trooper.com"},
                                     {user, "trooper"},
                                     {id_rsa, File}]).
3> trooper_ssh:exec(Trooper, "ls -lha").
{ok,0,
    <<"total 128K\ndrwxr-xr-x 10 trooper trooper 4.0K Mar  8 16:54"...>>}
12> trooper_ssh:stop(Trooper).
ok
```
Enjoy!


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/altenwald/trooper/blob/master/doc/trooper_keys.md" class="module">trooper_keys</a></td></tr>
<tr><td><a href="http://github.com/altenwald/trooper/blob/master/doc/trooper_ssh.md" class="module">trooper_ssh</a></td></tr></table>

