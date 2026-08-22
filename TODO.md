# Trooper Improvement Roadmap & TODO

This document outlines architectural improvements, code smells, and enhancements identified for future releases of Trooper.

---

## 1. Error Handling & Worker Lifecycle in `trooper_ssh`

- [ ] **Propagate failures in `exec_long_polling/2,3`**:
  - Replace unchecked pattern matching `{ok, Chan} = ssh_connection:session_channel(...)` with structured error handling that notifies the caller process (`Parent ! {error, Reason}`) before terminating.
  - On `ssh_connection:exec` failure, explicitly send `Parent ! {error, ExecError}` to the parent process rather than letting the worker process exit silently without notifying the receiver.
  - Review `spawn_link` usage to prevent remote channel rejections from taking down caller processes that do not trap exits.

---

## 2. Configurable Timeouts

- [ ] **Configurable Command & Stream Inactivity Timeout**:
  - Make `?COMMAND_TIMEOUT` (currently hardcoded to `60_000` ms in `get_and_send_all_info/3` and `trooper_proxy_chain:processing/2`) configurable via host options or allow `:infinity` for long-running idle processes and tail commands.
- [ ] **Configurable Proxy Setup Timeout**:
  - Make `?LISTENING_TIMEOUT` in `trooper_proxy` (currently hardcoded to `1000` ms) configurable via options to support slow bastion hops or higher-latency network connections.
- [ ] **Configurable SFTP Channel Timeouts**:
  - Allow custom timeouts when initiating channels and performing large file transfers in `trooper_scp`.

---

## 3. Idiomatic Error Returns in `trooper_proxy`

- [ ] **Replace `throw` with standard `{error, Reason}` tuples**:
  - Refactor `trooper_proxy:start/1,2` to return `{error, Reason}` instead of throwing exceptions on connection failures or timeouts, aligning with standard Erlang/OTP design conventions.

---

## 4. SFTP Channel Reuse & Transactions in `trooper_scp`

- [ ] **Add scoped SFTP transactions and sessions**:
  - Implement `trooper_scp:transaction/2` to allow executing multiple batch operations (e.g. uploading/downloading multiple files, directory traversal) over a single persistent SFTP channel without incurring per-operation channel open/close overhead.
  - Allow passing an existing SFTP channel pid directly to `trooper_scp` functions.

---

## 5. Differentiate `stdout` and `stderr`

- [ ] **Granular Output Stream Messages**:
  - In `get_and_send_all_info/3`, distinguish standard output (`Type = 0`) from standard error (`Type = 1`, extended data).
  - Provide an option to deliver `{continue, stdout, Data}` vs `{continue, stderr, Data}` or keep backward-compatible `{continue, Data}` based on caller configuration.

---

## 6. Modernize Supervisor & Proxy Architecture

- [ ] **Refactor `trooper_proxy_sup` and `trooper_proxy_chain`**:
  - Migrate away from the legacy `simple_one_for_one` supervisor strategy.
  - Replace `supervisor_bridge` with an idiomatic `gen_server` connection worker for socket bridging and lifecycle management.
