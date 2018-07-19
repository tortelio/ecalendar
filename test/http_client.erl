-module(http_client).

-export([connect/2,
         disconnect/1,
         get/3,
         put/4]).

%%%=============================================================================
%%% API
%%%=============================================================================

connect(Url, Port) ->
    {ok, _Conn} = gun:open(Url, Port).

disconnect(Conn) ->
    ok = gun:close(Conn).

%% @doc Send a GET request to the server and wait until a response is sent.
get(Conn, Path, Headers) ->
    Ref = gun:get(Conn, Path, Headers),
    % We should handle headers, status code, etc.
    % gun:await(Conn,Ref),
    {ok, Body} = gun:await_body(Conn, Ref),
    Body.

%% @doc Send a PUT request to the server and wait until a response is sent.
put(Conn, Path, Header, Body) ->
    Ref = gun:put(Conn, Path, Header, Body),
    {response, _IsFin, Status, _Headers} = gun:await(Conn, Ref),
    Status.
