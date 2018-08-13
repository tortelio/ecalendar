-module(ecalendar_SUITE).
-include("ecalendar_test.hrl").

-compile(export_all).

all() -> [get_calendar,
          get_calendar_with_unauthorized_user,
          add_event,
          get_report_of_event,
          add_event_with_unauthorized_user,
          delete_event,
          update_event,
          create_new_user,
          create_existing_user,
          delete_not_existing_user,
          delete_existing_user
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
    ecalendar_db:create_user(<<"testuser1">>, <<"password">>),
    Config2 = ecalendar_test:setup_http_connection(Config1),

    Config2.

end_per_testcase(_, Config1) ->
    ecalendar_db:delete_user(<<"testuser1">>),
    _Config2 = ecalendar_test:teardown_http_connection(Config1),

    ok.

%%------------------------------------------------------------------------------
%% TESTCASES
%%------------------------------------------------------------------------------

%% @doc Create a not existing user.
create_new_user(Config) ->
    ?assertEqual(false, ecalendar_db:user_exists(<<"testuser2">>)),
    {ok, _} = ecalendar_db:create_user(<<"testuser2">>, <<"password">>),

    ?assertEqual(true, ecalendar_db:user_exists(<<"testuser2">>)),
    {ok, _} = ecalendar_db:delete_user(<<"testuser2">>),

    ok.

%% @doc Create an existing user.
create_existing_user(Config) ->
    ?assertEqual(true, ecalendar_db:user_exists(<<"testuser1">>)),
    {error, _} = ecalendar_db:create_user(<<"testuser1">>, <<"password">>),

    ok.

%% @doc Delete a not existing user.
delete_not_existing_user(Config) ->
    ?assertEqual(false, ecalendar_db:user_exists(<<"testuser3">>)),
    {error, _} = ecalendar_db:delete_user(<<"testuser3">>),

    ok.

%% @doc Delete an existing user.
delete_existing_user(Config) ->
    {ok, _} = ecalendar_db:create_user(<<"testuser4">>, <<"password">>),
    ?assertEqual(true, ecalendar_db:user_exists(<<"testuser4">>)),

    BaseDir = code:priv_dir(ecalendar),
    {ok, OpenedFile} = file:open(filename:join([BaseDir, <<"data/testuser4/calendar/valami.ics">>]), [write, binary]),
    file:write(OpenedFile, <<"testuser4's event.">>),
    file:close(OpenedFile),
    {ok, _} = ecalendar_db:delete_user(<<"testuser4">>),

    ?assertEqual(false, ecalendar_db:user_exists(<<"testuser4">>)),

    ok.

%% @doc User sends a request with proper credentials to the server and get own calendar
get_calendar(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),
    Headers = ecalendar_test:authorization_headers(<<"testuser1">>, <<"password">>),
    ReqBody = <<"<D:propfind xmlns:D=\"DAV:\"><D:prop><D:getcontenttype/><D:resourcetype/><D:getetag/></D:prop></D:propfind>">>,
    {Code, _} = http_client:custom_request(ConnPid, <<"PROPFIND">>, "/testuser1/calendar", Headers, ReqBody),

    %% write assertions about calendar content
    ?assertEqual(207, Code),

    ok.

%% @doc User sends a request with wrong credentials to the server and get calendar
get_calendar_with_unauthorized_user(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),

    Headers = ecalendar_test:authorization_headers(<<"user-1">>, <<"bad-password-1">>),
    ReqBody = <<"<D:propfind xmlns:D=\"DAV:\"><D:prop><D:getcontenttype/><D:resourcetype/><D:getetag/></D:prop></D:propfind>">>,
    Reply = http_client:custom_request(ConnPid, <<"PROPFIND">>, "/user-1/calendar", Headers, ReqBody),

    ?assertEqual({401, undefined}, Reply),

    ok.

%% @doc User adds an event to his calendar
add_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"testuser1">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/testuser1/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({201, <<"CREATED">>}, Reply),

    EventExists = ecalendar_test:is_event_in_database(<<"valami.ics">>),

    ?assertEqual(true, EventExists),
    ok.

add_event_with_unauthorized_user(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"testuser1">>, <<"password-1">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/testuser1/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({401, undefined}, Reply),

    ok.

get_report_of_event(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),

    Headers = ecalendar_test:custom_headers(<<"testuser1">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/testuser1/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({201, <<"CREATED">>}, Reply),

    ReportHeader = ecalendar_test:custom_headers(<<"testuser1">>, <<"password">>, [{<<"content-type">>, <<"text/xml">>}]),
    ReportBody = ecalendar_test:get_report_request(<<"valami.ics">>),
    {Code, Reply2} = http_client:custom_request(ConnPid, <<"REPORT">>, "/testuser1/calendar", ReportHeader, ReportBody),

    ?assertEqual(207, Code),

    ok.

delete_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"testuser1">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/testuser1/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({201, <<"CREATED">>}, Reply),

    Etag = ecalendar_test:get_etag_of_event(<<"valami.ics">>),
    DeleteHeaders = ecalendar_test:custom_headers(<<"testuser1">>, <<"password">>, [{<<"if-match">>, Etag}]),
    Reply2 = http_client:delete(ConnPid, "/testuser1/calendar/valami.ics", DeleteHeaders),

    EventExists = ecalendar_test:is_event_in_database(<<"valami.ics">>),
    ?assertEqual({204, false}, {Reply2, EventExists}),

    ok.

update_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"testuser1">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/testuser1/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({201, <<"CREATED">>}, Reply),

    NewBody = <<"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:UPDATEDICSBODY\r\nEND:VCALENDAR">>,
    Etag = ecalendar_test:get_etag_of_event(<<"valami.ics">>),
    NewHeaders = ecalendar_test:custom_headers(<<"testuser1">>, <<"password">>, [{<<"if-match">>, Etag},
                                                                             {<<"content-type">>, <<"text/calendar">>}]),
    Reply2 = http_client:put(ConnPid, "/testuser1/calendar/valami.ics", NewHeaders, NewBody),

    ?assertEqual({201, <<"CREATED">>}, Reply2),

    ok.
