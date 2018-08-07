-module(ecalendar_xmlparse).

-export([create_response/3]).

create_response(Username, RequestBody, UserURI) ->
    {ok, Model} = erlsom:compile_xsd_file("priv/caldav.xsd"),
    RequestList = parse_request(RequestBody, Model),
    Response = get_requested_data(Username, UserURI, RequestList),
    {ok, OutPut} = erlsom:write(Response, Model),
    OutBin = binary:replace(binary:list_to_bin(OutPut), <<"><">>, <<">\r\n<">>, [global]),
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n", OutBin/binary>>.

parse_request(InputXML, Model) ->
    {ok, Result} = erlsom:parse(InputXML, Model),
    get_requested_elements(Result).

get_requested_elements(Request) ->
    List = tuple_to_list(Request),
    Filtered = lists:map(fun(L) -> filterfunction(L) end, List),
    lists:flatten(Filtered).

filterfunction(undefined) ->
    [];

filterfunction(Elem) ->
    case is_tuple(Elem) of
        false ->
            case is_list(Elem) of
                true when Elem =/= [] ->
                    [];
                _ ->
                    Elem
            end;
        true ->
            get_requested_elements(Elem)
    end.

get_requested_data(Cal, Uri, ReqList) ->
    ResponseBody = create_response_body(ReqList, Cal, Uri),
    {multistatus, [], ResponseBody, undefined, undefined}.

create_response_body([ReqForm | Rest], Cal, Uri) ->
    UserList = ets:match_object(calendar, {'_', ['_', '_', '_', Cal]}),
    case ReqForm of
        propfind ->
            get_propfind(Rest, UserList, Uri);
        'calendar-multiget' ->
            get_report(UserList)
    end.

get_propfind(ReqList, UserList, Uri) ->
    CalPropBody = create_prop_body(ReqList),
    EventData = get_event_responses(UserList, [], propfind),
    [{response, [], Uri, [{propstat, [], CalPropBody,  "HTTP/1.1 200 OK", undefined, undefined}
                         % {propstat, [], {prop, [], undefined, undefined, undefined, undefined, undefined,
                          %                "JJJ", undefined, undefined, undefined, undefined, undefined,
                         %                 undefined}, "HTTP/1.1 404 Not Found", undefined, undefined}
                         ],
      undefined, undefined, undefined} | EventData].

%% TODO
get_report(Userdata) ->
    get_event_responses(Userdata, [], report).

get_event_responses([], Acc, _) ->
    Acc;

get_event_responses([Current | Rest], Acc, Mode) ->
    {EventICS, [Body, Etag, Uri, Username]} = Current,
    {CalBody, ContType} = case Mode of
                              propfind ->
                                  {undefined, "text/calendar; charset=utf-8; component=vevent"};
                              report->
                                  {Body, undefined}
                          end,
    ThisEvent = create_event_prop(CalBody, ContType, Etag),
    UriPart = binary:bin_to_list(iolist_to_binary(Uri)),
    EventReport = {response, [], UriPart , [{propstat, [], ThisEvent, "HTTP/1.1 200 OK", undefined, undefined}], undefined, undefined, undefined},
    get_event_responses(Rest, [EventReport | Acc], Mode).

create_prop_body(ReqList) ->
    PropElements = [creationdate, displayname, getcontentlanguage, getcontentlength, getcontenttype, getetag, getlastmodified,
                    resourcetype, 'supported-report-set', 'quota-available-bytes','calendar-data', 'quota-used-bytes'],
    Start = {prop, []},
    create_prop_body(PropElements, ReqList, 3, Start).

create_prop_body([], _, _, Acc) ->
    Acc;

create_prop_body([CurrentProp | Rest], ReqList, Pos, Acc) ->
    ToInsert = case lists:member(CurrentProp, ReqList) of
                   false ->
                       undefined;
                   true ->
                       get_element(CurrentProp)
               end,
    NewAcc = erlang:insert_element(Pos, Acc, ToInsert),
    create_prop_body(Rest, ReqList, Pos+1, NewAcc).

get_element(Element) ->
    case Element of
        getcontenttype ->
            {getcontenttype, [], "httpd/unix-directory", []};
        resourcetype ->
            {resourcetype, [], {collection, []}, {calendar, []}, []};
        _ ->
            undefined
    end.

create_event_prop(CalBody, ContType, Etag) ->
    {prop, [], undefined, undefined, undefined, undefined, ContType,
     Etag, undefined, undefined, undefined, undefined, CalBody, undefined}.
