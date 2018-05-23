

# Module trooper_proxy_sup #
* [Description](#description)

Trooper Proxy Supervisor is in charge to start a supervisor under the
`trooper_app` supervisor.

__Behaviours:__ [`supervisor`](supervisor.md).

<a name="description"></a>

## Description ##

This way the supervisor is reachable via the
main supervisor in the application.

The supervisor is a simple one for one supervisor which is in charge to
add chains and ssh connections to be supervised.

The advantage with this is when we want to stop the connection we can
stop the supervisor and that's all. Even you can perform that task
via `trooper_proxy:stop/1` function.