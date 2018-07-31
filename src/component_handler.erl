
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
        generate_etag/3,
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
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>], Req, State}.

%% @doc Set the known methods for this handler.
known_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

resource_exists(Req, State) ->
    Filename = cowboy_req:binding(component, Req),
    IsExists = case ets:lookup(jozsical, Filename) of
                   [] -> false;
                   _ -> true
               end,
    {IsExists, Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_accepted(Req,State)->
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
    Method = cowboy_req:method(Req),
    {ReturnCode, ReturnBody} = handle_request(Method, Req),
    Req0 = cowboy_req:reply(ReturnCode, #{}, ReturnBody, Req),
    {ok, Req0, State}.

%%====================================================================
%% Internal functions
%%====================================================================

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

generate_etag(Path, Size, Mtime) ->
    {strong, integer_to_binary(erlang:phash2({Path, Size, Mtime}, 16#ffffffff))}.

handle_request(<<"PUT">>, Req) ->
    Uri = cowboy_req:uri(Req),
    Filename = cowboy_req:binding(component, Req),
    #{path := Path} = Req,
    Length = cowboy_req:parse_header(<<"content-length">>, Req),
    Last_mod_date = calendar:local_time(),
    {_ , Etag} = generate_etag(Path, Length, Last_mod_date),
    {ok, Body2, _} = read_body(Req, <<"">>),
    ets:insert(jozsical, {Filename, [Body2, Etag, Uri]}),
    {201, <<"CREATED">>};

handle_request(<<"GET">>, Req) ->
    Filename = cowboy_req:binding(component, Req),
    [{Filename, Got_data} | _ ] = ets:lookup(jozsical, Filename),
    [ReturnValue | _] = Got_data,
    {200, ReturnValue}.
