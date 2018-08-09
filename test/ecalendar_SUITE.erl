-module(ecalendar_SUITE).
-include("ecalendar_test.hrl").

-compile(export_all).

all() -> [get_calendar,
          %get_calendar_with_unauthorized_user,
          add_event,
          %get_report_of_event,
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
filelib:ensure_dir("data/jozsi/"),
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
ecalendar_user:create(<<"jozsi">>, <<"password">>),
    Config2 = ecalendar_test:setup_http_connection(Config1),

    Config2.

end_per_testcase(_, Config1) ->
ecalendar_user:delete(<<"jozsi">>),
    _Config2 = ecalendar_test:teardown_http_connection(Config1),

    ok.

%%------------------------------------------------------------------------------
%% TESTCASES
%%------------------------------------------------------------------------------

%% @doc Create a not existing user.
create_new_user(Config) ->
    ?assertEqual(false, ecalendar_user:exists(<<"mari">>)),
    {ok, _} = ecalendar_user:create(<<"mari">>, <<"password">>),

    ?assertEqual(true, ecalendar_user:exists(<<"mari">>)),

    ok.

%% @doc Create an existing user.
create_existing_user(Config) ->
    ?assertEqual(true, ecalendar_user:exists(<<"jozsi">>)),
    {error, _} = ecalendar_user:create(<<"jozsi">>, <<"password">>),

    ok.

%% @doc Delete a not existing user.
delete_not_existing_user(Config) ->
    ?assertEqual(false, ecalendar_user:exists(<<"bela">>)),
    {error, _} = ecalendar_user:delete(<<"bela">>),

    ok.

%% @doc Delete an existing user.
delete_existing_user(Config) ->
    {ok, _} = ecalendar_user:create(<<"janos">>, <<"password">>),
    ?assertEqual(true, ecalendar_user:exists(<<"janos">>)),

    {ok, OpenedFile} = file:open(<<"data/janos/event.ics">>, [write, binary]),
    file:write(OpenedFile, <<"janos's event.">>),
    file:close(OpenedFile),
    {ok, _} = ecalendar_user:delete(<<"janos">>),

    ?assertEqual(false, ecalendar_user:exists(<<"janos">>)),
    ?assertEqual(undefined, ets:info(flora)),

    ok.

%% @doc User sends a request with proper credentials to the server and get own calendar
get_calendar(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),    
    Headers = ecalendar_test:authorization_headers(<<"jozsi">>, <<"password">>),
    {Code, Reply} = http_client:custom_request(ConnPid, <<"PROPFIND">>, "/jozsi/calendar", Headers, <<"">>),
    CheckXML = ecalendar_test:is_xml_response(Reply),
    %% write assertions about calendar content
    ?assertEqual({207, true}, {Code, CheckXML}),

    ok.

%% @doc User sends a request with wrong credentials to the server and get calendar
get_calendar_with_unauthorized_user(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),

    Headers = ecalendar_test:authorization_headers(<<"user-1">>, <<"bad-password-1">>),
    Reply = http_client:custom_request(ConnPid, <<"PROPFIND">>, "/user-1/calendar", Headers, <<"">>),

    ?assertEqual({401, undefined}, Reply),

    ok.

%% @doc User adds an event to his calendar
add_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"jozsi">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/jozsi/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({201, <<"CREATED">>}, Reply),

    EventExists = ecalendar_test:is_event_in_database(<<"valami.ics">>),

    ?assertEqual(true, EventExists),
    ok.

add_event_with_unauthorized_user(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"jozsi">>, <<"password-1">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/jozsi/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({401, undefined}, Reply),

    ok.

%get_report_of_event(Config) ->
    %ConnPid = ecalendar_test:get_http_connection(Config),

    %Headers = ecalendar_test:custom_headers(<<"jozsi">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    %TestBody = ecalendar_test:get_test_putbody(),
    %Reply = http_client:put(ConnPid, "/jozsi/calendar/valami.ics", Headers, TestBody),

    %?assertEqual({201, <<"CREATED">>}, Reply),

    %ReportHeader = ecalendar_test:custom_headers(<<"jozsi">>, <<"password">>, [{<<"content-type">>, <<"text/xml">>}]),
    %ReportBody = ecalendar_test:get_report_request(<<"valami.ics">>),
    %{Code, Reply2} = http_client:custom_request(ConnPid, <<"REPORT">>, "/jozsi/calendar", ReportHeader, ReportBody),

    %?assertEqual(207, Code),

    %ok.

delete_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"jozsi">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/jozsi/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({201, <<"CREATED">>}, Reply),

    Etag = ecalendar_test:get_etag_of_event(<<"valami.ics">>),
    DeleteHeaders = ecalendar_test:custom_headers(<<"jozsi">>, <<"password">>, [{<<"if-match">>, Etag}]),
    Reply2 = http_client:delete(ConnPid, "/jozsi/calendar/valami.ics", DeleteHeaders),

    EventExists = ecalendar_test:is_event_in_database(<<"valami.ics">>),
    ?assertEqual({204, false}, {Reply2, EventExists}),

    ok.

update_event(_Config) ->
    ConnPid = ecalendar_test:get_http_connection(_Config),

    Headers = ecalendar_test:custom_headers(<<"jozsi">>, <<"password">>, [{<<"content-type">>, <<"text/calendar">>}]),
    TestBody = ecalendar_test:get_test_putbody(),
    Reply = http_client:put(ConnPid, "/jozsi/calendar/valami.ics", Headers, TestBody),

    ?assertEqual({201, <<"CREATED">>}, Reply),

    NewBody = <<"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:UPDATEDICSBODY\r\nEND:VCALENDAR">>,
    Etag = ecalendar_test:get_etag_of_event(<<"valami.ics">>),
    NewHeaders = ecalendar_test:custom_headers(<<"jozsi">>, <<"password">>, [{<<"if-match">>, Etag},
                                                                             {<<"content-type">>, <<"text/calendar">>}]),
    Reply2 = http_client:put(ConnPid, "/jozsi/calendar/valami.ics", NewHeaders, NewBody),

    ?assertEqual({201, <<"CREATED">>}, Reply2),

    ok.
