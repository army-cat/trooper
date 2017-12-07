

# Trooper #

Copyright (c) 2016-2017 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/altenwald/trooper/master.svg)](https://travis-ci.org/altenwald/trooper)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/trooper.svg)](https://codecov.io/gh/altenwald/trooper)
[![License: LGPL 2.1](https://img.shields.io/github/license/altenwald/trooper.svg)](https://raw.githubusercontent.com/altenwald/trooper/master/COPYING)
[![Hex](https://img.shields.io/hexpm/v/trooper.svg)](https://hex.pm/packages/trooper)

Trooper is a soldier in charge to go to other machines (via SSH) and follow your commands.


### <a name="Requirements">Requirements</a> ###

Trooper requires to be run over an Erlang/OTP 19+.

| Erlang Version | Support | Notes |
|:---|:---:|:---|
| 20.1 | :heavy_check_mark: | Recommended if you use OTP 20 |
| 20.0 | :heavy_check_mark: | |
| 19.3 | :heavy_check_mark: | Recommended if you use OTP 19 |
| 19.2 | :heavy_check_mark: | |
| 19.1 | :heavy_check_mark: | |
| 19.0 | :heavy_check_mark: | |


### <a name="Example">Example</a> ###

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
Enjoy!


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="trooper_keys.md" class="module">trooper_keys</a></td></tr>
<tr><td><a href="trooper_proxy.md" class="module">trooper_proxy</a></td></tr>
<tr><td><a href="trooper_ssh.md" class="module">trooper_ssh</a></td></tr></table>

