%% @doc Trooper keys implements the `ssh_client_key_api' behaviour to give a
%%      solution to handle the keys without files and ensuring all of the
%%      connections are validated instead of create a known hosts file.
%%
%%      Using the connection options is in charge to ensure the user is using
%%      a certificate passed in those options and even the password to access
%%      to that key.
%%
%%      We can configure this in diferent ways:
%%
%%      <pre lang="erlang"><![CDATA[
%%      % config for inline certificate (without password)
%%      {id_rsa, <<"-----BEGIN RSA PRIVATE KEY-----\nMIIE..."},
%%      % or from a file
%%      {id_rsa, {file, "id_rsa"}},
%%      % and adding a password:
%%      {rsa_pass_phrase, <<"mypass">>},
%%      ]]></pre>
%%
%%      You can do that with `rsa', `dsa' and `ecdsa' algorithms. Keep in
%%      mind the `ecdsa' algorithm uses the `dsa_pass_phrase' configuration
%%      for the password.
%% @end
-module(trooper_keys).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ssh_client_key_api).

-export([
    add_host_key/3,
    is_host_key/4,
    user_key/2
]).


-spec add_host_key(string(), public_key:public_key(),
                   [proplists:property()]) -> ok.
%% @doc adds a trusted host key. In this implementation the addition is not
%%      done because all of the hosts are intended to be accepted.
%% @end
add_host_key(_HostNames, _Key, _ConnectOptions) -> ok.


-spec is_host_key(public_key:public_key(), Host :: string(),
                  ssh:pubkey_alg(),
                  [proplists:property()]) -> false.
%% @doc Is a trusted host key? The answer is always no (false) to force to the
%%      system to use add_host_key/3.
%% @end
is_host_key(_Key, _Host, _Algorithm, _ConnectOptions) -> false.


-spec user_key(ssh:pubkey_alg(), proplists:proplist()) ->
      {ok, public_key:private_key()} | {error, term()}.
%% @doc fetch the user public key. It's retrieved from the options.
user_key(Algorithm, ConnectOptions) ->
    PrivateOpts = proplists:get_value(key_cb_private, ConnectOptions, []),
    Type = get_algo(Algorithm),
    Phrase = get_algo_pass(Type),
    Password = proplists:get_value(Phrase, ConnectOptions, ignore),
    case proplists:get_value(Type, PrivateOpts, false) of
        false ->
            {error, notfound};
        {file, PEM} ->
            case file:read_file(PEM) of
                {ok, Certificate} -> decode(Certificate, Password);
                {error, Reason} -> {error, Reason}
            end;
        Certificate ->
            decode(Certificate, Password)
    end.

% Internal functions

-type algorithms() :: id_rsa | id_dsa | id_ecdsa.
-type algorithm_phrases() :: rsa_pass_phrase |
                             dsa_pass_phrase |
                             ecdsa_pass_phrase.

-spec get_algo(ssh_client_key_api:public_key_algorithm()) -> algorithms().
%% @doc translate the incoming algorithm to the specific key to retrieve the
%%      algorithm from the options.
%% @private
get_algo('ssh-rsa') -> id_rsa;
get_algo('ssh-dss') -> id_dsa;
get_algo(_) -> id_ecdsa.


-spec get_algo_pass(algorithms()) -> algorithm_phrases().
%% @doc retrieve the key for retrieve the password given the key of the
%%      algorithm used.
%% @private
get_algo_pass(id_rsa) -> rsa_pass_phrase;
get_algo_pass(id_dsa) -> dsa_pass_phrase;
get_algo_pass(id_ecdsa) -> dsa_pass_phrase.


-spec decode_ssh_cert(binary(), binary() | string()) ->
    ssh_client_key_api:private_key().
%% @doc decode the given certificate and the password to do it.
%% @private
decode_ssh_cert(Pem, Password) ->
    case public_key:pem_decode(Pem) of
        [{_,_,not_encrypted} = Entry] ->
            public_key:pem_entry_decode(Entry);
        [Entry] when Password =/= ignore ->
            public_key:pem_entry_decode(Entry, Password);
        _ ->
            throw("No pass phrase provided for private key file")
    end.

-type reason() :: atom() | string().

-spec decode(binary(), binary() | string()) ->
      {ok, ssh_client_key_api:private_key()} |
      {error, reason()}.
%% doc decodes the certificate with the given password.
%% @private
decode(Certificate, Password) ->
    try
        {ok, decode_ssh_cert(Certificate, Password)}
    catch
        throw:Reason ->
            {error, Reason};
        error:Reason ->
            {error, Reason}
    end.
