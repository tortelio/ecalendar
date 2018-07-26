%%%-------------------------------------------------------------------
%%% @doc Handles the requests to "/" on the server.
%%%-------------------------------------------------------------------

-module(calendar_handler).

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
        propfind_calendar/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Switch to REST handler behavior.
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {cowboy_rest, cowboy_req:req(), any()}.
init(Req0=#{method := <<"PROPFIND">>}, State) ->
    Body = get_ets_data(jozsical, ets:first(jozsical), <<"">>),
    Req = cowboy_req:reply(200, #{}, Body, Req0),
    {ok, Req, State};

init(Req,Opts)->
    {cowboy_rest,Req,Opts}.

%% NOTE: These callbacks seems useless so far, because PROPFIND requests are
%% handled at init/2, and no other request should be handled by this handler
%% @doc Set the allowed http methods for this handler.
allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"PROPFIND">>], Req, State}.

%% @doc Set the allowed http methods for this handler.
known_methods(Req, State) ->
    {[<<"PUT">>, <<"PROPFIND">>], Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_accepted(Req,State)->
    io:format("CONT"),
    {[
        {{<<"text">>, <<"xml">>, '*'}, propfind_calendar},
        {{<<"text">>, <<"calendar">>, '*'}, propfind_calendar}
    ],Req,State}.

%% @doc Media types provided by the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_provided(Req,State)->
    io:format("CONT2"),
    {[
        {{<<"text">>, <<"xml">>, []}, propfind_calendar},
        {{<<"text">>, <<"calendar">>, []}, propfind_calendar}
    ],Req,State}.

%% @doc Check the authorization of the request.
is_authorized(Req, State) ->
    io:format("AAA"),
    Username = cowboy_req:binding(username, Req),
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, Username, <<"password">>} ->
            {true, Req, Username};
        _ ->
            {{false, <<"Basic realm=\"Access to the staging site\"">>}, Req, State}
    end.

%% @doc Send back a simple response based on the method of the request.
-spec propfind_calendar(Req :: cowboy_req:req(), binary()) -> {{binary()}, cowboy_req:req(), any()}.
propfind_calendar(Req, State) ->
    Body2 = <<"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nEND:VCALENDAR">>,
    Req0 = {true, cowboy_req:reply(200, #{}, Body2, Req)},
    {"", Req0, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc a recursive function that concatenates all the element of an ets
get_ets_data(Cal, Key, Acc) ->
    case Key of
        '$end_of_table' ->
            Acc;
        _ ->
            [{Key, Return} | _] = ets:lookup(Cal, Key),
            get_ets_data(Cal, ets:next(Cal, Key), << Acc/binary, Return/binary >>)
    end.
