%%%-------------------------------------------------------------------
%%% @doc Handles the XML response generation on the server.
%%%-------------------------------------------------------------------

-module(ecalendar_transform).

%%====================================================================
%% Exports
%%====================================================================

-export([start/0,
         create_response/3,
         skip_auth/1]).

%%====================================================================
%% API
%%====================================================================

start() ->
    PrivDir = code:priv_dir(ecalendar),
    CalDav = lists:concat([PrivDir, "/caldav.xsd"]),
    {ok, Model} = erlsom:compile_xsd_file(CalDav,
                                          [{include_files,
                                            [{"urn:ietf:params:xml:ns:caldav", "C", lists:concat([PrivDir, "/ns2.xsd"])},
                                             {"DAV:", "D", CalDav}]}]),
    ets:new(model, [set, named_table, public]),
    ets:insert(model, {caldav, Model}),
    ok.

%% @doc Creates the XML response for the request
create_response(Username, RequestBody, UserURI) ->
    Model = get_model(),
    RequestList = parse_request(RequestBody, Model),
    Response = get_requested_data(Username, UserURI, RequestList),
    {ok, OutPut} = erlsom:write(Response, Model),
    binary:replace(binary:list_to_bin(OutPut), <<"><">>, <<">\r\n<">>, [global]).

skip_auth(RequestBody) ->
    Model = get_model(),
    RequestList = parse_request(RequestBody, Model),
    lists:member(owner, RequestList).

%%====================================================================
%% Internal functions
%%====================================================================

get_model() ->
    [{_, Model}] = ets:lookup(model, caldav),
    Model.

parse_request(InputXML, Model) ->
    {ok, Result} = erlsom:parse(InputXML, Model),
    get_requested_elements(Result).

%% @doc Creates a list containing the requested elements
get_requested_elements(Request) ->
    List = tuple_to_list(Request),
    Filtered = lists:map(fun(L) -> filterfunction(L) end, List),
    lists:flatten(Filtered).

%% @doc Function used to filter the parsed request XML
filterfunction(undefined) ->
    [];

filterfunction(Elem) ->
    case is_tuple(Elem) of
        false ->
            Elem;
        true ->
            get_requested_elements(Elem)
    end.

%% @doc Creates the response
get_requested_data(Cal, Uri, ReqList) ->
    ResponseBody = create_response_body(ReqList, Cal, Uri),
    {multistatus, [], ResponseBody, undefined, undefined}.

%% @doc Creates the body for the response based on the request list
create_response_body([ReqForm | Rest], CalUser, CalUri) ->
    UserList = ecalendar_db:get_user_list(CalUser),
    case ReqForm of
        propfind ->
            case lists:member(owner, Rest) of
                true ->
                    get_ctag_response(CalUser, CalUri);
                false ->
                    get_propfind(Rest, UserList, iolist_to_binary(CalUri), CalUser)
            end;
        'C:calendar-multiget' ->
            get_event_responses(UserList, [], report)
    end.

get_propfind(ReqList, UserList, Uri, User) ->
    CalPropBody = create_prop_body(ReqList, Uri, User),
    EventData = case lists:member('C:calendar-home-set', ReqList) of
                    false ->
                        get_event_responses(UserList, [], propfind);
                    true ->
                        []
                end,
    [{response, [], Uri, [{propstat, [], CalPropBody,  "HTTP/1.1 200 OK", undefined, undefined}],
      undefined, undefined, undefined} | EventData].

%% @doc Collects the details of the events
get_event_responses([], Acc, _) ->
    Acc;

get_event_responses([Current | Rest], Acc, Mode) ->
    {Uri, [Body, Etag, _, _]} = Current,
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

create_prop_body(ReqList, Uri, User) ->
    PropElements = [resourcetype, owner, 'current-user-principal', 'supported-report-set', 'supported-calendar-component-set', 
                    getcontentlength, getcontenttype, getetag, 'C:calendar-home-set', 'C:calendar-user-address-set',
                    'calendar-data', getctag, 'C:schedule-inbox-URL', 'C:schedule-outbox-URL'],
    Start = {prop, []},
    create_prop_body(PropElements, ReqList, Uri, User, 3, Start).

create_prop_body([], _, _, _, _, Acc) ->
    Acc;

create_prop_body([CurrentProp | Rest], ReqList, Uri, User, Pos, Acc) ->
    ToInsert = case lists:member(CurrentProp, ReqList) of
                   false ->
                       undefined;
                   true ->
                       get_element(CurrentProp, Uri, User)
               end,
    NewAcc = erlang:insert_element(Pos, Acc, ToInsert),
    create_prop_body(Rest, ReqList, Uri, User, Pos+1, NewAcc).

get_element(Element, Uri, User) ->
    case Element of
        resourcetype ->
            {resourcetype, [], {collection, []}, {'C:calendar', []}, []};
        'C:calendar-home-set' ->
            {'C:calendar-home-set', [], Uri};
        'C:calendar-user-address-set' ->
            {'C:calendar-user-address-set', [], Uri};
        'C:schedule-inbox-URL' ->
            {'C:schedule-inbox-URL', [], <<Uri/binary, "inbox">>};
        'C:schedule-outbox-URL' ->
            {'C:schedule-outbox-URL', [], <<Uri/binary, "outbox">>};
        _ ->
            undefined
    end.

create_event_prop(CalBody, ContType, Etag) ->
    {prop, [], undefined, undefined, undefined, undefined, undefined,
     undefined, ContType, Etag, undefined, undefined, CalBody, undefined, undefined, undefined}.

%% @doc Creates a response for the ctag request
get_ctag_response(CalUser, CalURI) ->
    ShortURI = <<CalUser/binary, "/calendar/">>,
    PropBody = ctag_prop_body(CalUser, CalURI),
    [{response, [], ShortURI, [{propstat, [], PropBody, "HTTP/1.1 200 OK", undefined, undefined}], undefined, undefined, undefined}].
ctag_prop_body(User, Uri) ->
    Ctag = create_ctag(User),
    SuppComp = {'C:supported-calendar-component-set', [], [{'C:comp', [], "VEVENT"}]},
    SuppReports = get_reports(['calendar-multiget', 'calendar-query', 'free-busy-query', 'read-free-busy'], []),
    {prop, [], {resourcetype, [], {collection, []}, {'C:calendar', []}, []}, {owner, [], binary:bin_to_list(Uri)},
     {'current-user-principal',[], binary:bin_to_list(Uri)}, {'supported-report-set', [], SuppReports, []},
     SuppComp, undefined, undefined, undefined, undefined, undefined, undefined, Ctag, undefined, undefined}.

%% @doc Collects the supported reports
get_reports([], Acc) ->
    lists:reverse(Acc);

get_reports([Current | Rest], Acc) ->
    This = {'supported-report', [], {report, [], {Current, [], undefined, undefined}}},
    get_reports(Rest, [This | Acc]).

%% @doc Concatenate all of the etags from the ets and then creates the Ctag for the calendar
-spec create_ctag(Username :: binary()) -> binary().
create_ctag(Username) ->
    UserList = ecalendar_db:get_user_list(Username),
    create_ctag(UserList, <<"">>).

-spec create_ctag([], Username :: binary()) -> binary().
create_ctag([UserListHead | UserListTail], Acc) ->
    {_, [_, Etag, _, _]} = UserListHead,
    create_ctag(UserListTail, <<Acc/binary, Etag/binary>>);

create_ctag([], Acc) ->
    base64:encode(Acc).
