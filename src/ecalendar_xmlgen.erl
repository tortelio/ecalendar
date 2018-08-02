%%%-------------------------------------------------------------------
%%% @doc this module generates the xml response bodies for the server
%%%-------------------------------------------------------------------

-module(ecalendar_xmlgen).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([create_report/1,
         create_propfind/1
        ]).

%%====================================================================
%% API
%%====================================================================

%% @doc Build an xml REPORT response.
-spec create_report(Cal :: atom()) -> binary().
create_report(Cal) ->
    ResponseBegin = get_xml_header(<<"multistatus">>),
    ResponseBody = create_report_body(Cal, ets:first(Cal), ResponseBegin),
    <<ResponseBody/binary, "\r\n</d:multistatus>">>.

%% @doc Create an xml PROPFIND response.
-spec create_propfind(Cal :: atom()) -> binary().
create_propfind(Cal) ->
    ResponseBegin = get_xml_header(<<"multistatus">>),
    ResponseBody = get_prop_body(Cal),
    <<ResponseBegin/binary, ResponseBody/binary>>.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc The beginning of the response xml.
-spec get_xml_header(State :: any()) -> binary().
get_xml_header(State) ->
    <<"<?xml version=\"1.0\" encoding=\"utf-8\" ?><d:", State/binary, " xmlns:d=\"DAV:\"
    xmlns:CS=\"http://calendarserver.org/ns/\"
    xmlns:C=\"urn:ietf:params:xml:ns:caldav\">\r\n">>.

%% @doc The beginning of an ics response in the REPORT xml.
-spec get_report_header(Etag :: atom(), Eri :: atom()) -> binary().
get_report_header(Etag, Uri) ->
    <<"\r\n<d:response>\r\n<d:href>", Uri/binary , "</d:href>\r\n<d:propstat>\r\n<d:prop><d:getetag>", Etag/binary, "</d:getetag>
      <C:calendar-data>">>.

%% @doc Build a response xml from all the ics in the ets.
-spec create_report_body(Cal :: atom(), Key :: atom(), Acc :: binary()) -> binary().
create_report_body(Cal, Key, Acc) ->
    PartFin = <<"</C:calendar-data></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response>">>,
    case Key of
        '$end_of_table' ->
            Acc;
        _ ->
            [{Key, [Return, Etag, Uri | _]} | _] = ets:lookup(Cal, Key),
            Header = get_report_header(Etag, iolist_to_binary(Uri)),
            create_report_body(Cal, ets:next(Cal, Key), <<Acc/binary, Header/binary, Return/binary, PartFin/binary>>)
    end.

%% @doc Build an xml response body for a PROPFIND request.
-spec get_prop_body(Cal :: atom()) -> binary().
get_prop_body(Cal) ->
    Body1 = <<"<d:response><d:href>http://localhost:8080/", Cal/binary, "/calendar</d:href><d:propstat><d:prop><d:getcontenttype>
    httpd/unix-directory</d:getcontenttype><d:resourcetype><d:collection /><C:calendar/></d:resourcetype></d:prop>
    <d:status>HTTP/1.1 200 OK</d:status></d:propstat><d:propstat><d:prop><d:getetag /></d:prop>
    <d:status>HTTP/1.1 404 Not Found</d:status></d:propstat></d:response>">>,
    Body2 = get_user_details(Cal, Body1),
    <<Body2/binary, "</d:multistatus>">>.

%% @doc Recursive function that builds the end of the PROPFIND reponse xml from the ets
get_user_details(Cal, Acc) ->
    get_user_details(Cal, ets:first(binary_to_atom(Cal, utf8)), Acc).

get_user_details(Cal, Key, Acc) ->
    case Key of
        '$end_of_table' ->
            Acc;
        _ ->
            Start = <<"<d:response><d:href>http://localhost:8080/", Cal/binary, "/calendar/", Key/binary,
            "</d:href><d:propstat><d:prop><d:getcontenttype>text/calendar; charset=utf-8; component=vevent
            </d:getcontenttype><d:getetag>">>,
            [{Key, [Return, Etag, Uri | _]} | _] = ets:lookup(binary_to_atom(Cal, utf8), Key),
            BodyRest = <<Etag/binary, "</d:getetag></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response>">>,
            get_user_details(Cal, ets:next(binary_to_atom(Cal, utf8), Key), <<Acc/binary, Start/binary, BodyRest/binary>>)
    end.
