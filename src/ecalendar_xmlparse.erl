%%%-------------------------------------------------------------------
%%% @doc Handles the XML response generation on the server.
%%%-------------------------------------------------------------------

-module(ecalendar_xmlparse).

%%====================================================================
%% Exports
%%====================================================================

-export([create_response/3]).

%%====================================================================
%% API
%%====================================================================

create_response(Username, RequestBody, UserURI) ->
    {ok, Model} = erlsom:compile_xsd_file("priv/caldav.xsd",
                                          [{include_files,
                                            [{"urn:ietf:params:xml:ns:caldav", "C", "priv/ns2.xsd"},
                                             {"DAV:", "D", "priv/caldav.xsd"}]}]),
    RequestList = parse_request(RequestBody, Model),
    io:format("~p~n", [Model]),
    Response = get_requested_data(Username, UserURI, RequestList),
    io:format("~p~n", [Response]),
    {ok, OutPut} = erlsom:write(Response, Model),
    OutBin = binary:replace(binary:list_to_bin(OutPut), <<"><">>, <<">\r\n<">>, [global]),
    %file:write_file("Ki.xml", OutBin),
    OutBin.
    %<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n", OutBin/binary>>.

%%====================================================================
%% Internal functions
%%====================================================================

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
            Elem;
        true ->
            get_requested_elements(Elem)
    end.

get_requested_data(Cal, Uri, ReqList) ->
    ResponseBody = create_response_body(ReqList, Cal, Uri),
    {multistatus, [], ResponseBody, undefined, undefined}.

create_response_body([ReqForm | Rest], CalUser, CalUri) ->
    UserList = ets:match_object(calendar, {'_', ['_', '_', '_', CalUser]}),
    case ReqForm of
        propfind ->
            case lists:member(resourcetype, Rest) =:= false or lists:member('supported-report-set', Rest) of
                true ->
                    get_ctag_response(CalUser, CalUri);
                _ ->
                    get_propfind(Rest, UserList, CalUri)
            end;
        'C:calendar-multiget' ->
            get_event_responses(UserList, [], report)
    end.

get_propfind(ReqList, UserList, Uri) ->
    CalPropBody = create_prop_body(ReqList),
    EventData = get_event_responses(UserList, [], propfind),
    [{response, [], Uri, [{propstat, [], CalPropBody,  "HTTP/1.1 200 OK", undefined, undefined}],
      undefined, undefined, undefined} | EventData].

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
    PropElements = [resourcetype, owner, 'current-user-principal', 'supported-report-set', 'supported-calendar-component-set', 
                    getcontentlength, getcontenttype, getetag, getlastmodified,
                    'quota-available-bytes','calendar-data', "TEST"],
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
        resourcetype ->
            {resourcetype, [], {collection, []}, {'C:calendar', []}, []};
        _ ->
            undefined
    end.

create_event_prop(CalBody, ContType, Etag) ->
    {prop, [], undefined, undefined, undefined, undefined, undefined,
     undefined, ContType, Etag, undefined, undefined, CalBody, undefined}.

get_ctag_response(CalUser, CalURI) ->
    ShortURI = <<CalUser/binary, "/calendar/">>,
    PropBody = ctag_prop_body(CalUser, CalURI),
    [{response, [], ShortURI, [{propstat, [], PropBody, "HTTP/1.1 200 OK", undefined, undefined}], undefined, undefined, undefined}].
ctag_prop_body(User, Uri) ->
    Ctag = create_ctag(User),
    SuppComp = {'C:supported-calendar-component-set', [], [{'C:comp', [], "VEVENT"}]},
    SuppReports = get_reports(['calendar-multiget', 'calendar-query', 'free-busy-query'], []),
    {prop, [], {resourcetype, [], {collection, []}, {'C:calendar', []}, []}, {owner, [], binary:bin_to_list(Uri)},
     {'current-user-principal',[], binary:bin_to_list(Uri)}, {'supported-report-set', [], SuppReports, []},
     SuppComp, undefined, undefined, undefined, undefined, undefined, undefined, Ctag}.

get_reports([], Acc) ->
    lists:reverse(Acc);

get_reports([Current | Rest], Acc) ->
    This = {'supported-report', [], {report, [], {Current, [], undefined, undefined}}},
    get_reports(Rest, [This | Acc]).

%% @doc Concatenate all of the etags from the ets and then creates the Ctag for the calendar
-spec create_ctag(Username :: binary()) -> binary().
create_ctag(Username) ->
    UserList = ets:match_object(calendar, {'_', ['_', '_', '_', Username]}),
    create_ctag(UserList, <<"">>).

-spec create_ctag([], Username :: binary()) -> binary().
create_ctag([UserListHead | UserListTail], Acc) ->
    {Filename, [Body, Etag, Uri, Username]} = UserListHead,
    create_ctag(UserListTail, <<Acc/binary, Etag/binary>>);

create_ctag([], Acc) ->
    base64:encode(Acc).
