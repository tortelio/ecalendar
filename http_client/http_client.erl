-module(http_client).

-export([connect/2,
         disconnect/1,
         get/2]).

-export([send_4_reply/3,
         send_4_redirection/3]).

connect(Url, Port) ->
    {ok, Conn} = gun:open(Url, Port),

    {ok, Conn}.

disconnect(Conn) ->
    ok = gun:close(Conn).

get(Conn, Path) ->
    Ref = gun:get(Conn, Path),
    {ok, Reply} = gun:await_body(Conn, Ref),
    Reply.
