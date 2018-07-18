-module(ecalendar_test).

-export([authorization_headers/2]).

authorization_headers(Username, Password) ->
    EncodedCredentials = base64:encode(<<Username/binary, ":", Password/binary>>),
    [{<<"Authorization">>, <<"Basic ", EncodedCredentials/binary>>}].
