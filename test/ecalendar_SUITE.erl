-module(ecalendar_SUITE).

-include("ecalendar_test.hrl").

-compile(export_all).

all() -> [get_calendar,
          get_calendar_with_unauthorized_user,
          add_event,
          get_report_of_event,
          add_event_with_unauthorized_user,
          delete_event,
          multiple_user_event,
          get_freebusy_information,
          update_event
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
    %ok = application:stop(ecalendar),
    ok.

%%------------------------------------------------------------------------------
%% TESTCASE init/end
%%------------------------------------------------------------------------------

init_per_testcase(multiple_user_event, Config1) ->
    ecalendar_db:create_user(<<"testuserB">>, <<"password">>, <<"test@test.com">>),
    init_per_testcase(common, Config1);

init_per_testcase(_, Config1) ->
    ecalendar_db:create_user(<<"testuser">>, <<"password">>, <<"test@test.com">>),
    Config2 = ecalendar_test:setup_http_connection(Config1),

    Config2.

end_per_testcase(_, Config1) ->
    ok = ecalendar_db:drop(),
    _Config2 = ecalendar_test:teardown_http_connection(Config1),

    ok.

%%------------------------------------------------------------------------------
%% TESTCASES
%%------------------------------------------------------------------------------

%% @doc User sends a request with proper credentials to the server and get own calendar
get_calendar(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),
    Headers = ecalendar_test:authorization_headers(<<"testuser">>, <<"password">>),
    ReqBody = <<"<D:propfind xmlns:D=\"DAV:\"><D:prop><D:getcontenttype/><D:resourcetype/><D:getetag/></D:prop></D:propfind>">>,
    {Code, _} = http_client:custom_request(ConnPid, <<"PROPFIND">>, "/testuser/calendar", Headers, ReqBody),

    %% write assertions about calendar content
    ?assertEqual(207, Code),

    ok.

%% @doc User sends a request with wrong credentials to the server and get calendar
get_calendar_with_unauthorized_user(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),

    Headers = ecalendar_test:authorization_headers(<<"user-1">>, <<"bad-password-1">>),
    ReqBody = <<"<D:propfind xmlns:D=\"DAV:\"><D:prop><D:getcontenttype/><D:resourcetype/><D:getetag/></D:prop></D:propfind>">>,

    ?assertEqual({401, undefined}, http_client:custom_request(ConnPid, <<"PROPFIND">>, "/user-1/calendar", Headers, ReqBody)),

    ok.

%% @doc User adds an event to his calendar
add_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"testuser">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),

    ?assertEqual({201, <<"CREATED">>}, http_client:put(ConnPid, "/testuser/calendar/valami.ics", Headers, TestBody)),

    ?assertEqual(true, ecalendar_db:event_exists(<<"/testuser/calendar/valami.ics">>)),
    ok.

add_event_with_unauthorized_user(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"testuser">>, <<"password-1">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),

    ?assertEqual({401, undefined}, http_client:put(ConnPid, "/testuser/calendar/valami.ics", Headers, TestBody)),

    ok.

get_report_of_event(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),

    Headers = ecalendar_test:custom_headers(<<"testuser">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),

    ?assertEqual({201, <<"CREATED">>}, http_client:put(ConnPid, "/testuser/calendar/valami.ics", Headers, TestBody)),

    ReportHeader = ecalendar_test:custom_headers(<<"testuser">>, <<"password">>, [{<<"content-type">>, <<"text/xml">>}]),
    ReportBody = ecalendar_test:get_report_request(<<"valami.ics">>),
    {Code, Reply2} = http_client:custom_request(ConnPid, <<"REPORT">>, "/testuser/calendar", ReportHeader, ReportBody),

    ?assertEqual(207, Code),

    ok.

delete_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"testuser">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),

    ?assertEqual({201, <<"CREATED">>}, http_client:put(ConnPid, "/testuser/calendar/valami.ics", Headers, TestBody)),

    Etag = ecalendar_test:get_etag_of_event(<<"/testuser/calendar/valami.ics">>),
    DeleteHeaders = ecalendar_test:custom_headers(<<"testuser">>, <<"password">>, [{<<"if-match">>, Etag}]),

    ?assertEqual({204, false}, {http_client:delete(ConnPid, "/testuser/calendar/valami.ics", DeleteHeaders),
                                ecalendar_db:event_exists(<<"/testuser/calendar/valami.ics">>)}),

    ok.

update_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"testuser">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),

    ?assertEqual({201, <<"CREATED">>}, http_client:put(ConnPid, "/testuser/calendar/valami.ics", Headers, TestBody)),

    NewBody = <<"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:UPDATEDICSBODY\r\nEND:VCALENDAR">>,
    Etag = ecalendar_test:get_etag_of_event(<<"/testuser/calendar/valami.ics">>),
    NewHeaders = ecalendar_test:custom_headers(<<"testuser">>, <<"password">>, [{<<"if-match">>, Etag},
                                                                                {<<"content-type">>, <<"text/calendar">>}]),

    ?assertEqual({201, <<"CREATED">>}, http_client:put(ConnPid, "/testuser/calendar/valami.ics", NewHeaders, NewBody)),

    ok.

get_freebusy_information(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),
    Headers = ecalendar_test:custom_headers(<<"testuser">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:read_suite_data_file("test.ics", _Config),

    ?assertEqual({201, <<"CREATED">>}, http_client:put(ConnPid, "/testuser/calendar/test.ics", Headers, TestBody)),

    FreebusyBody = ecalendar_test:read_suite_data_file("freebusyrequest.ics", _Config),

    {Code, Resp} = http_client:post(ConnPid, "/testuser/outbox", Headers, FreebusyBody),
    BinList = binary:split(Resp, <<"FBTYPE=BUSY">>, [global]),

    ?assertEqual({200, 2}, {Code, length(BinList)}),

    ok.

multiple_user_event(_Config) ->
%%This testcase is for later use to test the interction between two users

    ConnPid = ecalendar_test:get_http_connection(_Config),
    HeadersA = ecalendar_test:custom_headers(<<"testuser">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    HeadersB = ecalendar_test:custom_headers(<<"testuserB">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),

    TestBody = ecalendar_test:read_suite_data_file("multiple_users.ics", _Config),

    ?assertEqual({201, <<"CREATED">>}, http_client:put(ConnPid, "/testuser/calendar/multiple_users.ics", HeadersA, TestBody)),

    ?assertEqual({201, <<"CREATED">>}, http_client:put(ConnPid, "/testuserB/calendar/multiple_users.ics", HeadersB, TestBody)),

    ?assertEqual({true, true}, {ecalendar_db:event_exists(<<"/testuser/calendar/multiple_users.ics">>),
                                ecalendar_db:event_exists(<<"/testuserB/calendar/multiple_users.ics">>)}),

    ok.
