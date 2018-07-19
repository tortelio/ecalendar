-module(ecalendar_SUITE).
-include("ecalendar_test.hrl").

-compile(export_all).

all() -> [get_calendar,
          get_calendar_with_unauthorized_user,
          add_event,
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

    Headers = ecalendar_test:authorization_headers(<<"user-1">>, <<"password-1">>),
    Reply = http_client:get(ConnPid, "/user-1/calendar", Headers),

    %% write assertions about calendar content
    ?assertEqual({200, <<"OK">>}, Reply),

    ok.

%% @doc User sends a request with wrong credentials to the server and get calendar
get_calendar_with_unauthorized_user(Config) ->
    ConnPid = ecalendar_test:get_http_connection(Config),

    Headers = ecalendar_test:authorization_headers(<<"user-1">>, <<"bad-password-1">>),
    Reply = http_client:get(ConnPid, "/user-1/calendar", Headers),

    ?assertEqual({401, undefined}, Reply),

    ok.

add_event(_Config) ->
    ok.

add_event_with_unauthorized_user(_Config) ->
    ok.
