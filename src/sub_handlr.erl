%%====================================================================
%% 
%%====================================================================

-module(sub_handlr).

%%====================================================================
%% Exports
%%====================================================================

-export([init/2,
         content_types_provided/2,
         html_response/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Call the REST.
init(Req,Opts)->
    {cowboy_rest,Req,Opts}.

%% @doc 
content_types_provided(Req,State)->
    {[
        {<<"text/html">>, html_response}
    ],Req,State}.

%% @doc
html_response(Req, State) ->
    Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>Response</title>
</head>
<body>
	<p>This is a simple Response</p>
</body>
</html>">>,
    {Body, Req, State}.
