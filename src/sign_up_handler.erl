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
    Password = cowboy_req:header(<<"password">>, Req0),
    Email = cowboy_req:header(<<"email">>, Req0),
    {RetCode, RetBody} = case lists:member(undefined, [Username, Password, Email]) of
                             true ->
                                 {400, <<"Missing header">>};
                             false ->
                                 {Ret, _} = ecalendar_db:create_user(Username, Password, Email),
                                 case Ret of
                                     ok ->
                                         {200, <<"User created.">>};
                                     error ->
                                         {409, <<"User already exists.">>}
                                 end
                         end,
    Req = cowboy_req:reply(RetCode, #{}, RetBody, Req0),
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

