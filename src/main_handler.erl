%%%-------------------------------------------------------------------
%%% @doc Handles the requests to "/" on the server.
%%%-------------------------------------------------------------------

-module(main_handler).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([init/2,
        allowed_methods/2,
        known_methods/2,
        is_authorized/2,
        content_types_accepted/2,
        content_types_provided/2,
        get_html/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Switch to REST handler behavior.
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {cowboy_rest, cowboy_req:req(), any()}.
init(Req,Opts)->
    {cowboy_rest,Req,Opts}.

%% @doc Set the allowed http methods for this handler.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"POST">>, <<"OPTIONS">>, <<"PROPFIND">>], Req, State}.

%% @doc Set the allowed http methods for this handler.
known_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"POST">>, <<"OPTIONS">>, <<"PROPFIND">>], Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_accepted(Req,State)->
    {[
        {<<"text/html">>, get_html},
        {<<"text/plain">>, get_html},
        {<<"text/calendar">>, get_html},
        {<<"text/xml">>, get_html}
    ],Req,State}.

%% @doc Media types provided b the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_provided(Req,State)->
    {[
        {<<"text/html">>, get_html},
        {<<"text/plain">>, get_html},
        {<<"text/calendar">>, get_html},
        {<<"text/xml">>, get_html}
    ],Req,State}.

%% @doc Check the authorization of the request.
is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User = <<"jozsi">>, <<"password">>} ->
            {true, Req, State};
        _ ->
            {{false, <<"Basic realm=\"Access to the staging site\"">>}, Req, State}
    end.

%% @doc Send back a simple response based on the method of the request.
-spec get_html(Req :: cowboy_req:req(), binary()) -> {{binary()}, cowboy_req:req(), any()}.
get_html(Req, State) ->
   {ok, Body2, Req0} = read_body(Req, <<"">>),
   io:format("HHH"),
   Method = cowboy_req:method(Req),
   case Method of
       <<"PROPFIND">> ->
           Body = <<"This is a response for a GET request.">>;
       <<"PUT">> ->
           Body = <<"A">>;
       _ ->
           Body = {true, cowboy_req:reply(200, #{}, <<"This is a response for a NON-GET request.">>, Req)}
   end,
   {Body2, Req, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

