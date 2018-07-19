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
        is_authorized/2,
		content_types_accepted/2,
        content_types_provided/2,
        get_html/2
        ]).

%%====================================================================
%% API
%%====================================================================

%% @doc Switch to REST handler behavior.
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {cowboy_rest, cowboy_req:req(), any()}.
init(Req,Opts)->
    {cowboy_rest,Req,Opts}.

%% @doc Set the allowed http methods for this handler.
allowed_methods(Req, State) ->
{[<<"GET">>, <<"PUT">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_accepted(Req,State)->
    {[
        {<<"text/html">>, get_html},
        {<<"text/plain">>, get_html}
    ],Req,State}.

%% @doc Media types provided b the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_provided(Req,State)->
    {[
        {<<"text/html">>, get_html},
        {<<"text/plain">>, get_html}
    ],Req,State}.

%% @doc Checks if User is authorized to send the request
is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User = <<"jozsi">>, <<"password">>} ->
            {true, Req, State};
        _ ->
            {{false, <<"Basic realm=\"Access to the staging site\"">>}, Req, State}
    end.

%% @doc Send back a simple html as a response based on the method of the request.
-spec get_html(Req :: cowboy_req:req(), binary()) -> {{binary()}, cowboy_req:req(), any()}.
get_html(Req, State) ->
    Method = cowboy_req:method(Req),
    Body = case Method of
        <<"GET">> ->
            <<"This is a response for a GET request.">>;
        _ ->
            {true, cowboy_req:reply(200, #{}, <<"This is a response for a NON-GET request.">>, Req)}
        end,
    {Body, Req, State}.

