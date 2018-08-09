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

%% @doc Handle an OPTIONS Request.
-spec init(Req :: cowboy_req:req(), State :: any()) -> {atom(), Req :: cowboy_req:req(), any()}.
init(Req0=#{method := <<"OPTIONS">>}, Opts) ->
    {cowboy_rest, Req0, Opts};

%% @doc Handles the PROPFIND and REPORT requests
init(Req0, State) ->
    Username = cowboy_req:binding(username, Req0),
    IsUser = filelib:is_dir(<<"data/", Username/binary>>),
    {RespCode, RespBody} = case IsUser of
                               true ->
                                   Uri = iolist_to_binary(cowboy_req:uri(Req0)),
                                   {ok, ReqBody, _} = read_body(Req0, <<"">>),
                                   Resp = ecalendar_xmlparse:create_response(Username, ReqBody, Uri),
                                   {207, Resp};
                               false ->
                                   Body = <<"NOT REGISTERED USER">>,
                                   {412, Body}
                           end,
    Req = cowboy_req:reply(RespCode, #{<<"DAV">> => <<"1, 2, 3 calendar-access, calendar-schedule, calendar-query">>}, RespBody, Req0),
    {ok, Req, State}.

%% @doc Set the allowed http methods for this handler.
-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Set the known http methods for this handler.
-spec known_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
known_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"DELETE">>, <<"GET">>, <<"PUT">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {[{{binary()}, atom()}], Req :: cowboy_req:req(), State :: any()}.
content_types_accepted(Req,State)->
    {[
        {{<<"text">>, <<"xml">>, '*'}, propfind_calendar}
    ],Req,State}.

%% @doc Media types provided by the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {[{{binary()}, atom()}], Req :: cowboy_req:req(), State :: any()}.
content_types_provided(Req,State)->
    {[
        {{<<"text">>, <<"xml">>, []}, propfind_calendar}
    ],Req,State}.

%% @doc Check the authorization of the request.
is_authorized(Req, State) ->
    Username = cowboy_req:binding(username, Req),
    [{Username, StoredPasswordHash}]= ets:lookup(authorization, Username),
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, Username, <<"password">>} ->
            {true, Req, State};
        _ ->
            {{false, <<"Basic realm=\"Access to the staging site\"">>}, Req, State}
    end.

%% @doc Send back a simple response based on the method of the request.
-spec propfind_calendar(Req :: cowboy_req:req(), State :: any()) -> {atom(), Req :: cowboy_req:req(), State :: any()}.
propfind_calendar(Req, State) ->
    {ok, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Read the request body.
-spec read_body(Req0 :: cowboy_req:req(), Acc :: binary()) -> {atom(), binary(), Req :: cowboy_req:req()}.
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.
