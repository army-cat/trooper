# Trooper

[![Erlang CI](https://github.com/altenwald/trooper/actions/workflows/erlang.yml/badge.svg)](https://github.com/altenwald/trooper/actions/workflows/erlang.yml)
[![Hex.pm Version](https://img.shields.io/hexpm/v/trooper.svg)](https://hex.pm/packages/trooper)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/trooper/)
[![Hex.pm Total Downloads](https://img.shields.io/hexpm/dt/trooper.svg)](https://hex.pm/packages/trooper)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://raw.githubusercontent.com/altenwald/trooper/master/LICENSE)
[![Paypal: Donation](https://img.shields.io/badge/paypal-donation-yellow)](https://www.paypal.com/donate/?hosted_button_id=XK6Z5XATN77L2)
[![Patreon: Donation](https://img.shields.io/badge/patreon-donation-yellow)](https://www.patreon.com/altenwald)

**Trooper** is an automation and remote execution library in Erlang. It connects to remote machines via SSH, performs commands (one-off, long-polling, interactive shells with PTY allocation), transfers files via SFTP/SCP, and supports multi-hop SSH proxy tunneling.

---

## Features

- **SSH Execution (`trooper_ssh`):**
  - Connect with passwords, RSA/DSA/ECDSA private keys (file paths or binary terms), or ssh-agent.
  - Run one-time commands (`exec/2,3`).
  - Run streaming and interactive commands via `exec_long_polling/2,3` with optional PTY allocation (`ptty_allow`).
  - Channel reuse with `start_chan/1` and `stop_chan/1`.
  - Scoped connections via `transaction/2`.
- **SFTP File Transfers (`trooper_scp`):**
  - Read and write remote files (`read_file/2`, `write_file/3`).
  - Directory management (`list_dir/2`, `make_dir/2`, `del_dir/2`).
  - File management (`delete/2`, `rename/3`, `make_symlink/3`).
  - Streaming file handles (`open/3`, `read/2`, `write/2`, `close/1`).
- **SSH Multi-hop Proxy Chains (`trooper_proxy`):**
  - Tunnel connections through one or more intermediate bastion/proxy hosts.
  - Transparent port bridging via TCP forwards.

---

## Installation

Add `trooper` to your `rebar.config` dependencies:

```erlang
{deps, [
    {trooper, "~> 1.2"}
]}.
```

Or for Elixir projects in `mix.exs`:

```elixir
def deps do
  [
    {:trooper, "~> 1.2"}
  ]
end
```

---

## Quick Examples

### One-shot Command Execution

```erlang
{ok, KeyData} = file:read_file("/path/to/id_rsa"),
Opts = [
    {host, "remote.server.com"},
    {user, "deploy"},
    {id_rsa, KeyData}
],
{ok, Trooper} = trooper_ssh:start(Opts),
{ok, 0, Output} = trooper_ssh:exec(Trooper, "uname -a"),
ok = trooper_ssh:stop(Trooper).
```

### Interactive Command with Long Polling & PTY

```erlang
Opts = [
    {host, "remote.server.com"},
    {user, "deploy"},
    {id_rsa, KeyData},
    {ptty_allow, true}
],
{ok, Trooper} = trooper_ssh:start(Opts),
WorkerPid = trooper_ssh:exec_long_polling(Trooper, "bash"),
WorkerPid ! {send, "echo 'Hello Trooper'\n"},
%% Incoming responses are sent as messages to the calling process:
%% {continue, <<"Hello Trooper\n">>}
%% {exit_status, 0}
%% closed
ok = trooper_ssh:stop(Trooper).
```

### SFTP Remote File Operations

```erlang
{ok, Trooper} = trooper_ssh:start(Opts),
ok = trooper_scp:write_file(Trooper, "/tmp/config.json", <<"{\"key\":\"value\"}">>),
{ok, Content} = trooper_scp:read_file(Trooper, "/tmp/config.json"),
{ok, Files} = trooper_scp:list_dir(Trooper, "/tmp"),
ok = trooper_scp:delete(Trooper, "/tmp/config.json"),
ok = trooper_ssh:stop(Trooper).
```

---

## Documentation

Full documentation is available on [HexDocs](https://hexdocs.pm/trooper).

---

## License

This project is licensed under the terms of the [MIT License](LICENSE).

---

## Support & Donations

If you find this project useful, you can support its development:

[![Donate with PayPal](https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=XK6Z5XATN77L2)
