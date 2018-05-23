

# Module trooper_proxy_chain #
* [Description](#description)

Trooper Proxy Chain is a module in charge of create a chain in the
proxy communication.

__Behaviours:__ [`supervisor_bridge`](supervisor_bridge.md).

<a name="description"></a>

## Description ##

A chain is a hop between the client and the final
server used to reach to the last one in the chain.

Each middle chain has two processes and two ports: the ssh connection
to the server with the specific port for the socket connection to the
remote server and a local TCP server (other process and port).

The last chain has only one process and one port to connect to the
final destination.

To simplify this uses `supervisor_bridge` instead of `gen_server`.