-module(ecalendar_test).

-include("ecalendar_test.hrl").

-export([setup_http_connection/1,
         teardown_http_connection/1,
         is_xml_response/1,
         get_test_putbody/0,
         custom_headers/3,
         read_suite_data_file/2,
         get_etag_of_event/1,
         authorization_headers/2,
         get_report_request/1,
         get_http_connection/1]).

setup_http_connection(Config) ->
    {ok, ConnPid} = http_client:connect("localhost", 8080),

    [{http_conn, ConnPid} | Config].

teardown_http_connection(Config) ->
    ConnPid = proplists:get_value(http_conn, Config),
    ok = http_client:disconnect(ConnPid),

    Config.

get_http_connection(Config) ->
    proplists:get_value(http_conn, Config).

custom_headers(Username, Password, InputHeaders) ->
    AuthHeader = authorization_headers(Username, Password),
    lists:merge(InputHeaders, AuthHeader).

authorization_headers(Username, Password) ->
    EncodedCredentials = base64:encode(<<Username/binary, ":", Password/binary>>),
    [{<<"Authorization">>, <<"Basic ", EncodedCredentials/binary>>}].

is_xml_response(Input) ->
    [FirstLine | _] = binary:split(Input, <<"\n">>),
    FirstLine =:= <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>.

get_test_putbody() ->
    <<"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:TESTICSBODY\r\nEND:VCALENDAR">>.

get_report_request(Filename) ->
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <C:calendar-multiget xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:caldav\"><D:prop><D:getetag/><C:calendar-data/>
    </D:prop><D:href>/testuser/calendar/", Filename/binary, "</D:href></C:calendar-multiget>">>.

get_etag_of_event(EventName) ->
    case ets:match_object(calendar, {EventName, ['_', '_', '_', '_']}) of
        [{EventName, [_, Etag, _, _]}] ->
            Etag;
        _ ->
            error
    end.

suite_data_dir(Config) -> ?config(data_dir, Config).

read_suite_data_file(Filename, Config) ->
    Path = filename:join([suite_data_dir(Config), Filename]),
    {ok, Content} = file:read_file(Path),
    binary:replace(Content, [<<"\r\n">>, <<"\n">>], <<"\r\n">>, [global]).
