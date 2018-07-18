-module(http_client).

-export([connect/2,
         disconnect/1,
         get/2]).

connect(Url, Port) ->
    {ok, _Conn} = gun:open(Url, Port).

disconnect(Conn) ->
    ok = gun:close(Conn).

get(Conn, Path) ->
    get(Conn, Path, []);
get(Conn, Path, Headers) ->
    Ref = gun:get(Conn, Path, Headers),
    {ok, Reply} = gun:await_body(Conn, Ref),
    Reply.
