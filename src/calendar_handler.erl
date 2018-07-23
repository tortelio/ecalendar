%%%-------------------------------------------------------------------
%%% @doc Handles the requests to "/" on the server.
%%%-------------------------------------------------------------------

-module(calendar_handler).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([init/2,
        allowed_methods/2,
        is_authorized/2,
        content_types_accepted/2,
        content_types_provided/2,
        get_ics/2,
        put_ics/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Switch to REST handler behavior.
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {cowboy_rest, cowboy_req:req(), any()}.
init(Req,Opts)->
    {cowboy_rest,Req,Opts}.

%% @doc Set the allowed http methods for this handler.
allowed_methods(Req, State) ->
{[<<"GET">>,<<"PROPFIND">>], Req, State}.

%% @doc Set the allowed http methods for this handler.
known_methods(Req, State) ->
{[<<"GET">>,<<"PROPFIND">>], Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_accepted(Req,State)->
    {[
        {<<"text/calendar">>, put_ics}
    ],Req,State}.

%% @doc Media types provided by the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_provided(Req,State)->
    {[
        {<<"text/calendar">>, calendar_ics}
    ],Req,State}.
    
%% @doc Check the authorization of the request.
is_authorized(Req, State) ->
    Username = cowboy_req:binding(username, Req),
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User = Username, <<"password">>} ->
            {true, Req, State};
        _ ->
            {{false, <<"Basic realm=\"Access to the staging site\"">>}, Req, State}
    end.

%% @doc Send back a simple response based on the method of the request.
-spec put_ics(Req :: cowboy_req:req(), binary()) -> {{binary()}, cowboy_req:req(), any()}.
put_ics(Req, State) ->
    Body = cowboy_req:method(Req),
  {Body,Req,State}.

%% @doc Send back a simple response based on the method of the request.
-spec get_ics(Req :: cowboy_req:req(), binary()) -> {{binary()}, cowboy_req:req(), any()}.
get_ics(Req, State) ->
    Body = cowboy_req:method(Req),
  {Body,Req,State}.
