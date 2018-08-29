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
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {atom(), Req :: cowboy_req:req(), Opts :: any()}.
init(Req,Opts) ->
    {cowboy_rest, Req, Opts}.

%% @doc Set the allowed methods for this handler.
-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"DELETE">>, <<"GET">>, <<"PUT">>], Req, State}.

%% @doc Set the known methods for this handler.
-spec known_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
known_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"DELETE">>, <<"GET">>, <<"PUT">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Check if the resource exists in the ets.
-spec resource_exists(Req :: cowboy_req:req(), State :: any()) -> {IsExists :: atom(), Req :: cowboy_req:req(), State :: any()}.
resource_exists(Req, State) ->
    Uri = iolist_to_binary(cowboy_req:uri(Req, #{host => undefined})),
    IsExists = ecalendar_db:event_exists(Uri),
    {IsExists, Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {[{{binary()}, atom()}], Req :: cowboy_req:req(), State :: any()}.
content_types_accepted(Req,State)->
    {[
        {{<<"text">>, <<"calendar">>, '*'}, calendar_component}
     ],Req,State}.

%% @doc Media types provided by the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {[{{binary()}, atom()}], Req :: cowboy_req:req(), State :: any()}.
content_types_provided(Req,State)->
    {[
        {{<<"text">>, <<"xml">>, []}, calendar_component}
     ],Req,State}.

realm() ->
    <<"Basic realm=\"Access to the staging site\"">>.

%% @doc Check the authorization of the request.
is_authorized(Req, State) ->
    Username = cowboy_req:binding(username, Req),
    case cowboy_req:parse_header(<<"authorization">>, Req, undefined) of
        {basic, Username, Password} ->
            case ecalendar_db:authenticate_user(Username, Password) of
                true ->
                    {true, Req, State};
                false ->
                    {{false, realm()}, Req, State}
            end;
        _ ->
            {{false, realm()}, Req, State}
    end.

%% @doc Send back a simple response based on the method of the request.
-spec calendar_component(Req :: cowboy_req:req(), any()) -> {atom(), cowboy_req:req(), any()}.
calendar_component(Req, State) ->
    Method = cowboy_req:method(Req),
    {ReturnCode, ReturnBody} = handle_request(Method, Req),
    Req0 = cowboy_req:reply(ReturnCode, #{}, ReturnBody, Req),
    {ok, Req0, State}.

%% @doc Delete the resource from the ets.
-spec delete_resource(Req :: cowboy_req:req(), any()) -> {atom(), cowboy_req:req(), any()}.
delete_resource(Req, State) ->
    Uri = iolist_to_binary(cowboy_req:uri(Req, #{host => undefined})),
    ecalendar_db:delete_event(Uri),
    {true, Req, State}.

%% @doc Generate etag for a DELETE request.
-spec generate_etag(Req :: cowboy_req:req(), any()) -> {Etag :: binary(), cowboy_req:req(), any()}.
generate_etag(Req, State) ->
    Uri = iolist_to_binary(cowboy_req:uri(Req, #{host => undefined})),
    CalendarList = ecalendar_db:get_component(Uri),
    Etag = lists:nth(2, CalendarList),
    {Etag, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Generate etag for a PUT request.
-spec create_etag(ParsedICS :: #{}, Length :: binary()) -> binary().
create_etag(ParsedICS, Length) ->
    EventMapList = maps:get(events, ParsedICS),
    Mtime = lists:nth(3, maps:get('last-modified', lists:nth(1, EventMapList))),
    UID = lists:nth(3, maps:get('uid', lists:nth(1, EventMapList))),
    Result = integer_to_binary(erlang:phash2({UID, Length, Mtime}, 16#ffffffff)),
    <<"\"", Result/binary, "\"">>.

%% @doc Recursive function to get the whole Request body.
-spec read_body(Req0 :: cowboy_req:req(), Acc :: binary()) -> {atom(), binary(), Req :: cowboy_req:req()}.
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
    end.

%% @doc This functon is called for a PUT Request.
-spec handle_request(binary(), Req :: cowboy_req:req()) -> {non_neg_integer(), binary()}.
handle_request(<<"PUT">>, Req) ->
    Username = cowboy_req:binding(username, Req),
    Uri = cowboy_req:uri(Req, #{host => undefined}),
    Filename = cowboy_req:binding(component, Req),
    {ok, Body2, _} = read_body(Req, <<"">>),
    ParsedBody = eics:decode(Body2),
    Uri2 = iolist_to_binary(Uri),
    Etag = create_etag(ParsedBody, cowboy_req:parse_header(<<"content-length">>, Req)),
    ecalendar_db:insert_event(Uri2, [Body2, Etag, Username, ParsedBody]),
    {201, <<"CREATED">>}.
