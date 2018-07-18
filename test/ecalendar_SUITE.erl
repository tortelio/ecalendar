-module(ecalendar_SUITE).
-include("ecalendar_test.hrl").

-compile(export_all).

all() -> [get_calendar,
          get_calendar_with_unregistered_user
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

init_per_testcase(_, Config) ->
    {ok, Conn} = http_client:connect("localhost", 8080),

    [{http_conn, Conn} | Config].

end_per_testcase(_, Config) ->
    Conn = proplists:get_value(http_conn, Config),
    ok = http_client:disconnect(Conn),

    ok.

%%------------------------------------------------------------------------------
%% TESTCASES
%%------------------------------------------------------------------------------

get_calendar(_Config) ->
    % User send a request to get own calendar from server
    Reply = http_client:get(Conn, "/"),

    ?assertEqual({200, "Ok"})

    %?asserEqual(ServerResponse, Requested data),
    %%
    %%Header of the request XML should be handled within the gun application
    %%See here: https://ninenines.eu/docs/en/gun/1.0/guide/http/
    %%
    %Reply=http_client:put(Conn, <<"
  %BEGIN: VCALENDAR
  %Version: 2.0
  %PRODID: -//Company//CalDAV Client// EN
  %BEGIN:VEVENT
  %UID:identifier
  %DTSTAMP:20180710T130000Z
  %DTSTART:20180710T180000Z
  %DTEND:20180710T200000Z
  %SUMMARY: Main Event
  %ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=ATTENDING;CN="A.user":mailto:a@example.com
  %ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=NEED-ACTION;CN="B.user":mailto:b@example.com
  %ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=NEED-ACTION;CN="C.user":mailto:c@example.com
  %END:VEVENT
  %END:VCALENDAR
  %">>), %User sends the put request for creating event with others
  %?asserEqual(ServerResponse, Sent data),
  %Reply2=http_client:send_mail(<<"bauerbence5@gmail.com">>, <<"Dear B!\r\nWe notify you that A
  %invited you to an event called Meeting on 2018. 08. 05. 10:00, respond to this email with a yes or no to accept
  %or to decline the invitation.">>),  %%Other(s) got email about the event
  %?assertEqual(ServerResponse, OK),
  %After the others sent their answer, the user updates the event
  %http_client:get(Conn, UpdatedEventData),
  ok.
