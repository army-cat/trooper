# Trooper #

Copyright (c) 2016-2025 Altenwald

__Authors:__ "Manuel Rubio ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![codecov](https://codecov.io/gh/army-cat/trooper/graph/badge.svg?token=Oa7Ak456tN)](https://codecov.io/gh/army-cat/trooper)
[![Hex](https://img.shields.io/hexpm/v/trooper.svg)](https://hex.pm/packages/trooper)

Trooper is a soldier in charge to go to other machines (via SSH) and follow your commands.

### Requirements

Trooper requires to be run over an Erlang/OTP 24+.

### Example

I think this could be a great small example to use a one-time running command:

```erlang
1> {ok, File} = file:read_file("/home/trooper/.ssh/id_rsa").
{ok,<<"-----BEGIN RSA PRIVATE KEY-----\nMIIE"...>>}
2> {ok, Trooper} = trooper_ssh:start([{host, "trooper.com"},
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

And now, you can also use `trooper_ssh:exec_long_polling/2`:

```erlang
6> f(Trooper),
   Opts = [{host, "trooper.com"},
           {user, "trooper"},
           {id_rsa, File},
           {ptty_allow, true}],
   trooper_ssh:start(Opts),
   ok.
ok
7> PID = trooper_ssh:exec_long_polling(Trooper, "bash").
<0.128.0>
8> flush().
{continue, "bash$"}
9> PID ! {send, "ls\n"},
   flush().
{continue, "ls\nfile1.txt\n"}
{continue, "bash$"}
10> PID ! {send, "exit\n"},
    flush().
{continue, "exit\n"}
{exit_status, 0}
closed
```

Enjoy!
