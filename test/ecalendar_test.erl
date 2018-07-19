-module(ecalendar_test).

-export([setup_http_connection/1,
         get_http_connection/1]).
-export([authorization_headers/2]).

setup_http_connection(Config) ->
    {ok, ConnPid} = http_client:connect("localhost", 8080),

    [{http_conn, ConnPid} | Config].

teardown_http_connection(Config) ->
    ConnPid = proplists:get_value(http_conn, Config),
    ok = http_client:disconnect(ConnPid),

    Config.

get_http_connection(Config) ->
    proplists:get_value(http_conn, Config).

authorization_headers(Username, Password) ->
    EncodedCredentials = base64:encode(<<Username/binary, ":", Password/binary>>),
    [{<<"Authorization">>, <<"Basic ", EncodedCredentials/binary>>}].
