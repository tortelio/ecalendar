%%%-------------------------------------------------------------------
%%% @doc Handles the requests to "/" on the server.
%%%-------------------------------------------------------------------

-module(outbox_handler).

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
         freebusy/2]).

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
    {[<<"OPTIONS">>, <<"POST">>], Req, State}.

%% @doc Set the known methods for this handler.
-spec known_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
known_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"DELETE">>, <<"GET">>, <<"PUT">>, <<"PROPFIND">>, <<"REPORT">>,<<"POST">>], Req, State}.

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
        {{<<"text">>, <<"calendar">>, '*'}, freebusy}
     ],Req,State}.

%% @doc Media types provided by the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {[{{binary()}, atom()}], Req :: cowboy_req:req(), State :: any()}.
content_types_provided(Req,State)->
    {[
        {{<<"text">>, <<"xml">>, []}, freebusy}
     ],Req,State}.

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

realm() ->
    <<"Basic realm=\"Access to the staging site\"">>.

freebusy(Req, State) ->
    {ok, Body, _} = read_body(Req, <<"">>),
    Body2 = ecalendar_transform:create_freebusy_response(ecalendar_freebusy:get_response_body(Body),
                                                         ecalendar_freebusy:get_recipient(Body)),
    Req0 = cowboy_req:reply(200, #{}, Body2, Req),
    {ok, Req0, State}.

%% @doc Recursive function to get the whole Request body.
-spec read_body(Req0 :: cowboy_req:req(), Acc :: binary()) -> {atom(), binary(), Req :: cowboy_req:req()}.
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
    end.

