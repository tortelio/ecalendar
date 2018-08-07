%%%-------------------------------------------------------------------
%%% @doc This module let users to sign up to the server.
%%%-------------------------------------------------------------------

-module(sign_up_handler).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([init/2,
         known_methods/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2]).

%%====================================================================
%% API
%%====================================================================

-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {atom(), Req :: cowboy_req:req(), Opts :: any()}.
init(Req0=#{method := <<"POST">>}, State) ->
    Username = cowboy_req:header(<<"username">>, Req0),
    case Username of
        undefined ->
            Body = <<"Missing username header">>,
            Req = cowboy_req:reply(400, #{}, Body, Req0);
        _ ->
            {_, Body} = ecalendar_user:create(Username),
            Req = cowboy_req:reply(200, #{}, Body, Req0)
    end,
    {ok, Req, State};

%% @doc Switch to REST handler behavior.
init(Req,Opts) ->
    {cowboy_rest, Req, Opts}.

%% @doc Set the allowed methods for this handler.
-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"PUT">>, <<"POST">>], Req, State}.

%% @doc Set the known methods for this handler.
-spec known_methods(Req :: cowboy_req:req(), State :: any()) -> {[binary()], Req :: cowboy_req:req(), State :: any()}.
known_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"DELETE">>, <<"GET">>, <<"PUT">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {[{{binary()}, atom()}], Req :: cowboy_req:req(), State :: any()}.
content_types_accepted(Req,State)->
    {[
      {{<<"text">>, <<"xml">>, '*'}, sign_up}
     ],Req,State}.

%% @doc Media types provided by the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {[{{binary()}, atom()}], Req :: cowboy_req:req(), State :: any()}.
content_types_provided(Req,State)->
    {[
      {{<<"text">>, <<"xml">>, []}, sign_up}
     ],Req,State}.

%%====================================================================
%% Internal functions
%%====================================================================

