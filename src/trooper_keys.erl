-module(trooper_keys).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ssh_client_key_api).

-export([
    add_host_key/3,
    is_host_key/4,
    user_key/2
]).

% adds a trusted host key
add_host_key(_HostNames, _Key, _ConnectOptions) -> ok.

% is a trusted host key?
is_host_key(_Key, _Host, _Algorithm, _ConnectOptions) -> true.

% fetch the user public key
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

get_algo('ssh-rsa') -> id_rsa;
get_algo('ssh-dss') -> id_dsa;
get_algo(_) -> id_ecdsa.

get_algo_pass(id_rsa) -> rsa_pass_phrase;
get_algo_pass(id_dsa) -> dsa_pass_phrase;
get_algo_pass(id_ecdsa) -> ecdsa_pass_phrase.

decode_ssh_cert(Pem, Password) ->
    case public_key:pem_decode(Pem) of
        [{_,_,not_encrypted} = Entry] ->
            public_key:pem_entry_decode(Entry);
        [Entry] when Password =/= ignore ->
            public_key:pem_entry_decode(Entry, Password);
        _ ->
            {error, "No pass phrase provided for private key file"}
    end.

decode(Certificate, Password) ->
    try
        {ok, decode_ssh_cert(Certificate, Password)}
    catch
        error:Reason ->
            {error, Reason}
    end.
