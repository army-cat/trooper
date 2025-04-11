# Trooper #

Copyright (c) 2016-2025 Altenwald

__Authors:__ "Manuel Rubio ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Hex](https://img.shields.io/hexpm/v/trooper.svg)](https://hex.pm/packages/trooper)

Trooper is a soldier in charge to go to other machines (via SSH) and follow your commands.

### Requirements

Trooper requires to be run over an Erlang/OTP 24+.

> #### Don't use DSA for OTP 23+ {: .warning}
>
> We found very difficult to put this working. But if you achieve it, please let us know how!

### Example

```erlang
1> {ok, File} = file:read_file("/home/trooper/.ssh/id_rsa").
{ok,<<"-----BEGIN RSA PRIVATE KEY-----\nMIIE"...>>}
2> {ok,Trooper} = trooper_ssh:start([{host, "trooper.com"},
                                     {user, "trooper"},
                                     {id_rsa, File}]), ok.
ok
3> trooper_ssh:exec(Trooper, "ls -lha").
{ok,0,
    <<"total 128K\ndrwxr-xr-x 10 trooper trooper 4.0K Mar  8 16:54"...>>}
4> trooper_ssh:exec(Trooper, "ls not_found").
{ok,2,
    <<"ls: cannot access not_found: No such file or directory\n">>}
5> trooper_ssh:stop(Trooper).
ok
```

You can use for the options whatever from [ssh:connect/3](http://erlang.org/doc/man/ssh.md#connect-3) options.

Enjoy!
