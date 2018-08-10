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
         is_authorized/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Handle an OPTIONS Request.
-spec init(Req :: cowboy_req:req(), State :: any()) -> {atom(), Req :: cowboy_req:req(), any()}.
init(Req0 = #{method := <<"OPTIONS">>}, Opts) ->
    {cowboy_rest, Req0, Opts};

%% @doc Handles the PROPFIND requests
init(Req0 = #{method := <<"PROPFIND">>}, State) ->
    case is_authorized(Req, State) of
        {true, Req, State} ->
            x;
        {{false, Realm}, Req, State} ->
            y
    end;

%TODO = #{method := <<"REPORT">>}

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

    % TODO: is this necessery ???
    Req = cowboy_req:reply(RespCode, #{<<"DAV">> => <<"1, 2, 3 calendar-access, calendar-schedule, calendar-query">>}, RespBody, Req0),

    {ok, Req, State}.

%% @doc Set the known http methods for this handler.
-spec known_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
known_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Set the allowed http methods for this handler.
-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

realm() ->
    <<"Basic realm=\"Access to the staging site\"">>.

%% @doc Check the authorization of the request.
is_authorized(Req, State) ->
    Username = cowboy_req:binding(username, Req),
    case cowboy_req:parse_header(<<"authorization">>, Req, undefined) of
        {basic, Username, Password} ->
            case authenticate(Username, Password) of
                true ->
                    {true, Req, State};
                false ->
                    {{false, realm()}, Req, State}
            end;
        undefined ->
            {{false, realm()}, Req, State}
    end.

authenticate(Username, Password) ->
    case ets:lookup(authorization, Username) of
        [{Username, Password}] ->
            true;
        [{Username, _}] ->
            false;
        [] ->
            false
    end

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
