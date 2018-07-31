-module(ecalendar_SUITE).
-include("ecalendar_test.hrl").

-compile(export_all).

all() -> [get_calendar,
          %get_calendar_with_unauthorized_user,
          add_event,
          get_report_of_event,
          add_event_with_unauthorized_user
         ].

%%------------------------------------------------------------------------------
%% SUITE init/end
%%------------------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ecalendar),
    {ok, _} = application:ensure_all_started(gun),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(gun),
    ok = application:stop(ecalendar),
    ok.

%%------------------------------------------------------------------------------
%% TESTCASE init/end
%%------------------------------------------------------------------------------

init_per_testcase(_, Config1) ->
    Config2 = ecalendar_test:setup_http_connection(Config1),

    Config2.

end_per_testcase(_, Config1) ->
    _Config2 = ecalendar_test:teardown_http_connection(Config1),

    ok.

%%------------------------------------------------------------------------------
%% TESTCASES
%%------------------------------------------------------------------------------

%% @doc User sends a request with proper credentials to the server and get own calendar
get_calendar(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),

    Headers = ecalendar_test:authorization_headers(<<"jozsi">>, <<"password">>),
    {Code, Reply} = http_client:get(ConnPid, "/jozsi/calendar/", Headers),

    CheckXML = ecalendar_test:is_xml_response(Reply),
    %% write assertions about calendar content
    ?assertEqual({207, true}, {Code, CheckXML}),

    ok.

%% @doc User sends a request with wrong credentials to the server and get calendar
get_calendar_with_unauthorized_user(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),

    Headers = ecalendar_test:authorization_headers(<<"user-1">>, <<"bad-password-1">>),
    Reply = http_client:get(ConnPid, "/user-1/calendar", Headers),

    ?assertEqual({401, undefined}, Reply),

    ok.

% @doc User adds an event to his calendar
add_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:content_type_headers(<<"jozsi">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/jozsi/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({201, <<"CREATED">>}, Reply),

    ok.

add_event_with_unauthorized_user(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:content_type_headers(<<"jozsi">>, <<"password-1">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/jozsi/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({401, undefined}, Reply),

    ok.

get_report_of_event(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),

    Headers = ecalendar_test:content_type_headers(<<"jozsi">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/jozsi/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({201, <<"CREATED">>}, Reply),

    ReportHeader = ecalendar_test:content_type_headers(<<"jozsi">>, <<"password">>, [{<<"content-type">>, <<"text/xml">>}]),
    ReportBody = ecalendar_test:get_report_request(<<"valami.ics">>),
    {Code, Reply2} = http_client:custom_request(ConnPid, <<"REPORT">>, "/jozsi/calendar", ReportHeader, ReportBody),

    %CheckXML = ecalendar_test:is_xml_response(Reply2),
    ?assertEqual(207, Code),

    ok.
