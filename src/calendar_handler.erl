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
        is_authorized/2,
        content_types_accepted/2,
        content_types_provided/2,
        propfind_calendar/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Switch to REST handler behavior.
-spec init(Req :: cowboy_req:req(), Opts :: any()) -> {cowboy_rest, cowboy_req:req(), any()}.
init(Req0=#{method := <<"PROPFIND">>}, State) ->
    Ctag = concat_etags(jozsical),
    {ok, IoBody, _} = read_body(Req0, <<"">>),
    ReqBody = binary:split(IoBody, <<"getetag">>),
    Body = case length(ReqBody) of
               1 -> propfind_xml(Ctag);
               _ -> ecalendar_xmlgen:create_propfind(jozsical, true)
           end,
    Req = cowboy_req:reply(207, #{<<"DAV">> => <<"1, 2, 3 calendar-access, calendar-schedule, calendar-query">>}, Body, Req0),
    {ok, Req, State};
    %%<<"DAV:">> =>  <<"1, 2, 3, calendar-access, addressbook, extended-mkcol">>

init(Req0=#{method := <<"REPORT">>}, State) ->
    Retur = ecalendar_xmlgen:create_report(jozsical),
    Req = cowboy_req:reply(207, #{}, Retur, Req0),
    {ok, Req, State};

init(Req,Opts)->
    {cowboy_rest,Req,Opts}.

%% NOTE: These callbacks seems useless so far, because PROPFIND requests are
%% handled at init/2, and no other request should be handled by this handler
%% @doc Set the allowed http methods for this handler.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Set the allowed http methods for this handler.
known_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"PROPFIND">>, <<"REPORT">>], Req, State}.

%% @doc Media types accepted by the server.
-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_accepted(Req,State)->
    {[
        {{<<"text">>, <<"xml">>, '*'}, propfind_calendar}
    ],Req,State}.

%% @doc Media types provided by the server.
-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {{binary()}, cowboy_req:req(), any()}.
content_types_provided(Req,State)->
    {[
        {{<<"text">>, <<"xml">>, []}, propfind_calendar},
        {{<<"text">>, <<"calendar">>, []}, propfind_calendar}
    ],Req,State}.

%% @doc Check the authorization of the request.
is_authorized(Req, State) ->
    Username = cowboy_req:binding(username, Req),
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, Username, <<"password">>} ->
            {true, Req, State};
        _ ->
            {{false, <<"Basic realm=\"Access to the staging site\"">>}, Req, State}
    end.

%% @doc Send back a simple response based on the method of the request.
-spec propfind_calendar(Req :: cowboy_req:req(), binary()) -> {{binary()}, cowboy_req:req(), any()}.
propfind_calendar(Req, State) ->
    %Method = cowboy_req:method(Req),
    %{ReturnCode, ReturnBody} = handle_request(Method, Req),
    %Req0 = cowboy_req:reply(ReturnCode, #{}, ReturnBody, Req),
    {ok, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================

concat_etags(Cal) ->
    concat_etags(Cal, ets:first(Cal), <<"">>).

concat_etags(Cal, Key, Acc) ->
case Key of
        '$end_of_table' ->
            base64:encode(Acc);
        _ ->
            [{Key, CalendarList} | _] = ets:lookup(Cal, Key),
            CurrentEtag = lists:nth(2, CalendarList),
            concat_etags(Cal, ets:next(Cal, Key), <<Acc/binary, CurrentEtag/binary>>)
    end.

propfind_xml(Ctag) ->
<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<D:multistatus xmlns:D=\"DAV:\" xmlns:CS=\"http://calendarserver.org/ns/\" xmlns:C=\"urn:ietf:params:xml:ns:caldav\">
<D:response>
<D:href>jozsi/calendar/</D:href>
<D:propstat>\r\n<D:prop>\r\n<D:resourcetype>\r\n<D:collection />\r\n<C:calendar />
</D:resourcetype>
<D:owner>
<D:href>http://localhost:8080/jozsi/calendar/
</D:href>
</D:owner>
<D:current-user-principal>
<D:href>http://localhost:8080/jozsi/calendar/
</D:href>
</D:current-user-principal>
<D:supported-report-set>
<D:supported-report>
<D:report>
<C:calendar-multiget />
</D:report>
</D:supported-report>
<D:supported-report>
<D:report>
<C:calendar-query />
</D:report>
</D:supported-report>
<D:supported-report>
<D:report>
<C:free-busy-query />
</D:report>
</D:supported-report>
</D:supported-report-set>
<C:supported-calendar-component-set>
<C:comp name=\"VEVENT\" />
</C:supported-calendar-component-set>
<CS:getctag>", Ctag/binary, "</CS:getctag>\r\n</D:prop>
<D:status>HTTP/1.1 200 OK</D:status>\r\n</D:propstat>\r\n</D:response>\r\n</D:multistatus>">>.

%% @doc read the request body
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.
