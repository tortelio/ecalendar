%%% ----------------------------------------------------------------------------
%%% Copyright (C)
%%% ----------------------------------------------------------------------------
%%%
%%% @doc A basic http client for test purposes.
%%%
%%% This is an http client that we use to
%%% test the functions of the ecalendar_app-module(http_client).

-module(http_client).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% API
-export([connect/2,
         disconnect/1,
         get/2,
         put/4]).

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Connect to the specified url on the specified port
connect(Url, Port) ->
    {ok, Conn} = gun:open(Url, Port),
    {ok, Conn}.

%% @doc Disconnect from the server in the argument.
disconnect(Conn) ->
    ok = gun:close(Conn).

%% @doc Send a GET request to the server and wait until a response is sent.
get(Conn, Path) ->
    Ref = gun:get(Conn, Path),
    {ok, Reply} = gun:await_body(Conn, Ref),
    Reply.
    
%% @doc Send a PUT request to the server and wait until a response is sent.
put(Conn, Path, Header, Body) ->
	Ref = gun:put(Conn, Path, Header, Body),
	{ok, Reply} = gun:await_body(Conn,Ref),
	Reply.
