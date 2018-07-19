-module(ecalendar_SUITE).
-include("ecalendar_test.hrl").

-compile(export_all).

all() -> [connect_test,
          use_case_test,
          succesful_login_test,
          failed_login_test,
          put_test
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
  Config.

end_per_testcase(_, _Config) ->
  ok.

%%------------------------------------------------------------------------------
%% TESTCASES
%%------------------------------------------------------------------------------

%%Tries to connect to the server and then disconnects
connect_test(_Config) ->
  {ok, Conn} = http_client:connect("localhost", 8080),
  http_client:disconnect(Conn).

%%Sends a get request with good login to the server and checks the response code
succesful_login_test(_Config) ->
  TestLoginData = base64:encode(<<"jozsi:password">>),
  {ok, Conn} = http_client:connect("localhost", 8080),
  Reply2 = http_client:get(Conn, "/", [{<<"authorization">>, <<"Basic ", TestLoginData/binary>>}]),
  ?assertEqual(200, Reply2),
  ok = http_client:disconnect(Conn).

%%Sends a get request with wrong login to the server and checks the response code
failed_login_test(_Config) ->
  TestLoginData = base64:encode(<<"jozsi:badpassword">>),
  {ok, Conn} = http_client:connect("localhost", 8080),
  Reply1 = http_client:get(Conn, "/", [{<<"authorization">>, <<"Basic ", TestLoginData/binary>>}]),
  ?assertEqual(401, Reply1),
  ok = http_client:disconnect(Conn).

%%Sends a put request to the server and checks the response code
put_test(_Config) ->
  TestLoginData = base64:encode(<<"jozsi:password">>),
  {ok, Conn} = http_client:connect("localhost", 8080),
  Reply1 = http_client:put(Conn, "/", [{<<"authorization">>, <<"Basic ", TestLoginData/binary>>},
                                       {<<"content-type">>, <<"text/plain">>}], <<"Put ICS here">>),
  ?assertEqual(200, Reply1),
  ok = http_client:disconnect(Conn).

use_case_test(_Config) ->
  {ok, Conn} = http_client:connect("localhost", 8080), %User connects to the server
  http_client:get(Conn, "/", []), %User requests the calendar data from server
  %?asserEqual(ServerResponse, Requested data),
  %%
  %%Header of the request XML should be handled within the gun application
  %%See here: https://ninenines.eu/docs/en/gun/1.0/guide/http/
  %%
  %Reply=http_client:put(Conn,"/", [{<<"authorization">>, <<"Basic ", TestLoginData/binary>>},
  %                            {<<"content-type">>, <<"text/plain">>}],
  %<<"
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
