-module(http_client).

-export([connect/2,
         disconnect/1,
         get/3,
         delete/3,
         custom_request/5,
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
    {response, _IsFin, Status, _Headers} = gun:await(Conn, Ref),

    Body1 =
        case Status of
            Status when Status >= 400 ->
                undefined;
            _ ->
                {ok, Body2} = gun:await_body(Conn, Ref),
                Body2
        end,

    {Status, Body1}.

%% @doc Send a PUT request to the server and wait until a response is sent.
put(Conn, Path, Header, Body) ->
    Ref = gun:put(Conn, Path, Header, Body),
    {response, _IsFin, Status, _Headers} = gun:await(Conn, Ref),
    case Status of
        Status when Status >= 400 ->
            {Status, undefined};
        _ ->
            {ok, RespBody} = gun:await_body(Conn, Ref),
            {Status, RespBody}
    end.

delete(Conn, Path, Header) ->
    Ref = gun:delete(Conn, Path, Header),
    {response, _IsFin, Status, _Headers} = gun:await(Conn, Ref),
    Status.

custom_request(Conn, Method, Path, Header, Body) ->
    Ref = gun:request(Conn, Method, Path, Header, Body),
    {response, _IsFin, Status, _Headers} = gun:await(Conn, Ref),
    case Status of
        Status when Status >= 400 ->
            {Status, undefined};
        _ ->
            {ok, RespBody} = gun:await_body(Conn, Ref),
            {Status, RespBody}
    end.
