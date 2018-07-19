%%%-------------------------------------------------------------------
%%% @doc Handles the requests to "/sub".
%%%-------------------------------------------------------------------

-module(sub_handlr).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([init/2,
         content_types_provided/2,
         html_response/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Switch to REST handler behavior.
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {cowboy_rest, cowboy_req:req(), any()}.
init(Req,Opts)->
    {cowboy_rest,Req,Opts}.

%% @doc Return the list of media types the resource provides in order of preference.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_provided(Req,State)->
    {[
        {<<"text/html">>, html_response}
    ],Req,State}.

%% @doc Sends back a simple html as a response.
-spec html_response(Req :: cowboy_req:req(), binary()) -> {{binary()}, cowboy_req:req(), any()}.
html_response(Req, State) ->
    Body =
		<<"This is a simple Response">>,
    {Body, Req, State}.

