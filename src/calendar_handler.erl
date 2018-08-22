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
         is_authorized/2
         ]).

%%====================================================================
%% API
%%====================================================================

%% @doc Handles the PROPFIND requests
-spec init(Req :: cowboy_req:req(), State :: any()) -> {atom(), Req :: cowboy_req:req(), any()}.
init(Req0 = #{method := <<"PROPFIND">>}, State) ->
    {ok, ReqBody, _} = read_body(Req0, <<"">>),
    SkipAuth = ecalendar_transform:skip_auth(ReqBody),
    Auth = case is_authorized(Req0, State) of
               {true, Req0, State} ->
                   true;
               {{false, Realm}, Req0, State} ->
                   SkipAuth
           end,
    Username = cowboy_req:binding(username, Req0),
    IsUser = ecalendar_db:user_exists(Username),
    {RespCode, RespBody} = case Auth and IsUser of
                               true ->
                                   Uri = iolist_to_binary(cowboy_req:uri(Req0)),
                                   Resp = ecalendar_transform:create_response(Username, ReqBody, Uri),
                                   {207, Resp};
                               false ->
                                   case Auth of
                                       true ->
                                           Body = <<"NOT REGISTERED USER">>,
                                           {412, Body};
                                       false ->
                                           {401, <<"">>}
                                   end
                           end,
    Req = cowboy_req:reply(RespCode, #{}, RespBody, Req0),
    {ok, Req, State};

%% @doc Handles the REPORT requests
init(Req0 = #{method := <<"REPORT">>}, State) ->
    Auth = case is_authorized(Req0, State) of
               {true, Req0, State} ->
                   true;
               {{false, Realm}, Req0, State} ->
                   false
           end,
    Username = cowboy_req:binding(username, Req0),
    IsUser = ecalendar_db:user_exists(Username),
    {RespCode, RespBody} = case Auth and IsUser of
                               true ->
                                   Uri = iolist_to_binary(cowboy_req:uri(Req0)),
                                   {ok, ReqBody, _} = read_body(Req0, <<"">>),
                                   Resp = ecalendar_transform:create_response(Username, ReqBody, Uri),
                                   {207, Resp};
                               false ->
                                   case Auth of
                                       true ->
                                           Body = <<"NOT REGISTERED USER">>,
                                           {412, Body};
                                       false ->
                                           {401, <<"">>}
                                   end
                           end,
    Req = cowboy_req:reply(RespCode, #{}, RespBody, Req0),
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
            case ecalendar_db:authenticate_user(Username, Password) of
                true ->
                    {true, Req, State};
                false ->
                    {{false, realm()}, Req, State}
            end;
        _ ->
            {{false, realm()}, Req, State}
    end.

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
