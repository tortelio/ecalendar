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
    %Body = get_ets_data(jozsical, ets:first(jozsical), <<"">>),
    {ok, Body} = file:read_file("src/resp.xml"),
    Req = cowboy_req:reply(207, #{<<"DAV:">> =>  <<"1, 2, 3, calendar-access, addressbook, extended-mkcol">>}, Body, Req0),
    {ok, Req, State};

init(Req0=#{method := <<"REPORT">>}, State) ->
    Body = get_ets_data(jozsical, ets:first(jozsical), <<"">>),
    {ok, Beg} = file:read_file("respbeg.xml"),
    Fin = <<"</D:multistatus>">>,
    %io:format(Body),
    Retur = <<Beg/binary, Body/binary, Fin/binary>>,
    Req = cowboy_req:reply(207, #{}, Retur, Req0),
    {ok, Req, State};

init(Req,Opts)->
    Method = cowboy_req:method(Req),
    io:format(Method),
    {cowboy_rest,Req,Opts}.

%% NOTE: These callbacks seems useless so far, because PROPFIND requests are
%% handled at init/2, and no other request should be handled by this handler
%% @doc Set the allowed http methods for this handler.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Set the allowed http methods for this handler.
known_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_accepted(Req,State)->
    io:format("CONT"),
    {[
        {{<<"text">>, <<"xml">>, '*'}, propfind_calendar}
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
    Username = cowboy_req:binding(username, Req),
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, Username, <<"password">>} ->
            {true, Req, State};
        _ ->
            {{false, <<"Basic realm=\"Access to the staging site\"">>}, Req, State}
    end.

%% @doc Send back a simple response based on the method of the request.
-spec propfind_calendar(Req :: cowboy_req:req(), binary()) -> {{binary()}, cowboy_req:req(), any()}.
propfind_calendar(Req, State) ->
    %Body2 = <<"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nEND:VCALENDAR">>,
    %Req0 = {true, cowboy_req:reply(200, #{}, Body2, Req)},
    Method = cowboy_req:method(Req),
    io:format("PPPFFF"),
    {ReturnCode, ReturnBody} = handle_request(Method, Req),
    Req0 = cowboy_req:reply(ReturnCode, #{}, ReturnBody, Req),
    {ok, Req0, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc a recursive function that concatenates all the element of an ets
handle_request(<<"REPORT">>, Req) ->
    [{jozsical, Return} | _] = ets:lookup(jozsical, <<"test.ics">>),
    {207, Return}.

get_ets_data(Cal, Key, Acc) ->
    {ok, Beg} = file:read_file("partbeg.xml"),
    {ok, Fin} = file:read_file("partend.xml"),
    case Key of
        '$end_of_table' ->
            Acc;
        _ ->
            [{Key, Return} | _] = ets:lookup(Cal, Key),
            get_ets_data(Cal, ets:next(Cal, Key), <<Acc/binary, Beg/binary, Return/binary, Fin/binary>>)
    end.
