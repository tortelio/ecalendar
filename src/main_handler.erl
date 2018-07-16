%%====================================================================
%% 
%%====================================================================

-module(main_handler).

%%====================================================================
%% Exports
%%====================================================================

-export([init/2,
         content_types_provided/2,
         get_html/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Call the REST.
init(Req,Opts)->
    {cowboy_rest,Req,Opts}.

%% @doc 
content_types_provided(Req,State)->
    {[
        {<<"text/html">>, get_html}
    ],Req,State}.

%% @doc
get_html(Req, State) ->
    Method = cowboy_req:method(Req),
    Body = case Method of
        <<"GET">> ->
            <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>Response</title>
</head>
<body>
	<p>This is a response for a GET request.</p>
</body>
</html>">>;
        _ ->
            <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>Response</title>
</head>
<body>
	<p>This is a response for a request other than GET.</p>
</body>
</html>">>
        end,
    {Body, Req, State}.

