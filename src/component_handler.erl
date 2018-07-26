
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
        calendar_component/2
        ]).

%%====================================================================
%% API
%%====================================================================

%% @doc Switch to REST handler behavior.
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {cowboy_rest, cowboy_req:req(), any()}.
init(Req,Opts)->
    io:format("INIT\r\n"),
    {cowboy_rest,Req,Opts}.

%% @doc Set the allowed methods for this handler.
allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

%% @doc Set the known methods for this handler.
known_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

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
    io:format("PROV\r\n"),
    {[
        {{<<"text">>, <<"xml">>, []}, calendar_component}
     ],Req,State}.

%% @doc Check the authorization of the request.
is_authorized(Req, State) ->
    Username = cowboy_req:binding(username, Req),
    io:format("AUTH\r\n"),
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
    Body2 = <<"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nEND:VCALENDAR">>,
    Body3 = {true, cowboy_req:reply(200, #{}, Body2, Req)},
    {Body2, Req, State}.
