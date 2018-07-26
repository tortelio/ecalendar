
%%%-------------------------------------------------------------------
%%% @doc Handles the requests to "/:username/calendar" on the server.
%%%-------------------------------------------------------------------

-module(component_handler).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([init/2,
        known_methods/2,
        allowed_methods/2,
        is_authorized/2,
        content_types_accepted/2,
        content_types_provided/2,
        resource_exists/2,
        calendar_component/2
        ]).

%%====================================================================
%% API
%%====================================================================

%% @doc Switch to REST handler behavior.
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {cowboy_rest, cowboy_req:req(), any()}.
init(Req,Opts)->
    {cowboy_rest, Req, Opts}.

%% @doc Set the allowed methods for this handler.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

%% @doc Set the known methods for this handler.
known_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

resource_exists(Req, State) ->
    Filename = cowboy_req:binding(component, Req),
    io:format(Filename),
    IsExists = case ets:lookup(jozsical, Filename) of
                   [] -> false;
                   _ -> true
               end,
    io:format(IsExists),
    {IsExists, Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_accepted(Req,State)->
    io:format("ACC\r\n"),
    {[
        {{<<"text">>, <<"calendar">>, '*'}, calendar_component}
     ],Req,State}.

%% @doc Media types provided by the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_provided(Req,State)->
    {[
        {{<<"text">>, <<"xml">>, []}, calendar_component}
     ],Req,State}.

%% @doc Check the authorization of the request.
is_authorized(Req, State) ->
    Username = cowboy_req:binding(username, Req),
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, Username, <<"password">>} ->
            {true, Req, State};
        _ ->
            {{false, <<"Basic realm=\"Access to the staging site\"">>}, Req, State}
    end.

%% @doc Send back a simple response based on the method of the request.
-spec calendar_component(Req :: cowboy_req:req(), binary()) -> {{binary()}, cowboy_req:req(), any()}.
calendar_component(Req, State) ->
    io:format("COMPON"),
    Method = cowboy_req:method(Req),
    ReturnBody = handle_request(Method, Req),
    Req0 = {true, cowboy_req:reply(200, #{}, ReturnBody, Req)},
    {"", Req0, State}.

%%====================================================================
%% Internal functions
%%====================================================================

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

handle_request(<<"PUT">>, Req) ->
    io:format("PUT"),
    Filename = cowboy_req:binding(component, Req),
    {ok, Body2, _} = read_body(Req, <<"">>),
    ets:insert(jozsical, {Filename, Body2}),
    <<"CREATED">>;

handle_request(<<"GET">>, Req) ->
    io:format("GET"),
    Filename = cowboy_req:binding(component, Req),
    [{Filename, ReturnValue} | _ ] = ets:lookup(jozsical, Filename),
    ReturnValue.
