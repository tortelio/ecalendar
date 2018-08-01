
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
        delete_resource/2,
        generate_etag/2,
        calendar_component/2
        ]).

%%====================================================================
%% API
%%====================================================================

%% @doc Switch to REST handler behavior.
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {cowboy_rest :: atom(), Req :: cowboy_req:req(), Opts :: any()}.
init(Req,Opts) ->
    {cowboy_rest, Req, Opts}.

%% @doc Set the allowed methods for this handler.
-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"DELETE">>, <<"GET">>, <<"PUT">>], Req, State}.

%% @doc Set the known methods for this handler.
-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
known_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"DELETE">>, <<"GET">>, <<"PUT">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Check if the resource exists in the ets.
-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) -> {IsExists :: atom(), Req :: cowboy_req:req(), State :: any()}.
resource_exists(Req, State) ->
    Filename = cowboy_req:binding(component, Req),
    IsExists = case ets:lookup(jozsical, Filename) of
                   [] -> false;
                   _ -> true
               end,
    {IsExists, Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {[{{binary()}, atom()}], Req :: cowboy_req:req(), State :: any()}.
content_types_accepted(Req,State)->
    {[
        {{<<"text">>, <<"calendar">>, '*'}, calendar_component}
     ],Req,State}.

%% @doc Media types provided by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {[{{binary()}, atom()}], Req :: cowboy_req:req(), State :: any()}.
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
-spec calendar_component(Req :: cowboy_req:req(), any()) -> {atom(), cowboy_req:req(), any()}.
calendar_component(Req, State) ->
    Method = cowboy_req:method(Req),
    {ReturnCode, ReturnBody} = handle_request(Method, Req),
    Req0 = cowboy_req:reply(ReturnCode, #{}, ReturnBody, Req),
    {ok, Req0, State}.

%% @doc Delete the resource from the ets.
-spec calendar_component(Req :: cowboy_req:req(), any()) -> {atom(), cowboy_req:req(), any()}.
delete_resource(Req, State) ->
    Filename = cowboy_req:binding(component, Req),
    ets:delete(jozsical, Filename),
    {true, Req, State}.

%% @doc Generate etag for a DELETE request.
-spec calendar_component(Req :: cowboy_req:req(), any()) -> {Etag :: binary(), cowboy_req:req(), any()}.
generate_etag(Req, State) ->
    Filename = cowboy_req:binding(component, Req),
    [{Filename, [Body, Etag, Uri | _]} | _] = ets:lookup(jozsical, Filename),
    {Etag, Req, State}.

%% @doc Generate etag for a PUT request.
-spec calendar_component(Req :: cowboy_req:req()) -> binary().
generate_etag(Req) ->
    #{path := Path} = Req,
    Mtime = {{2018, 8, 01}, {12, 00, 00}},
    Length = cowboy_req:parse_header(<<"content-length">>, Req),
    Result = integer_to_binary(erlang:phash2({Path, Length, Mtime}, 16#ffffffff)),
    <<"\"", Result/binary, "\"">>.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Recursive function to get the whole Request body.
-spec read_body(Req0 :: cowboy_req:req(), Acc :: binary()) -> {ok :: atom(), binary(), Req :: cowboy_req:req()}.
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
    end.
    
%% @doc This functon is called for a PUT Request.
-spec read_body(binary(), Req :: cowboy_req:req()) -> {non_neg_integer(), binary()}.
handle_request(<<"PUT">>, Req) ->
    Uri = cowboy_req:uri(Req),
    Filename = cowboy_req:binding(component, Req),
    Etag = generate_etag(Req),
    {ok, Body2, _} = read_body(Req, <<"">>),
    ets:insert(jozsical, {Filename, [Body2, Etag, Uri]}),
    {201, <<"CREATED">>};

%% @doc This functon is called for a GET Request.
-spec read_body(binary(), Req :: cowboy_req:req()) -> {non_neg_integer(), ReturnValue :: binary()}.
handle_request(<<"GET">>, Req) ->
    Filename = cowboy_req:binding(component, Req),
    [{Filename, Got_data} | _ ] = ets:lookup(jozsical, Filename),
    [ReturnValue | _] = Got_data,
    {200, ReturnValue}.
