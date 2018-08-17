%%%-------------------------------------------------------------------
%%% @doc Checks for Free-Busy availability and creates response ICS
%%%-------------------------------------------------------------------

-module(ecalendar_freebusy).

%%====================================================================
%% Exports
%%====================================================================

-export([get_response_body/1,
         get_recipient/1
         ]).

%%====================================================================
%% API
%%====================================================================

-spec get_response_body(binary()) -> binary().
get_response_body(RequestBody) ->
    Original = #{freebusy := #{attendee := Attendee} = FreeBusy} = eics:decode(RequestBody),
    Username = get_username(Attendee),
    {StartTime, EndTime} = get_start_end_time(FreeBusy, 3),
    EventTimes = lists:map(fun(L) -> get_start_end_time(L, 4) end, find_user_events(Username)),
    FreebusyComp = create_availability(EventTimes, {StartTime, EndTime}, <<"">>),
    Modified = #{data => Original#{method => "METHOD:REPLY\r\n",
                                   freebusy => FreeBusy#{response => binary:bin_to_list(FreebusyComp),
                                                         dtstamp => lists:concat(["DTSTAMP:", get_local_time(), "\r\n"])
                                                        }}},
    maps:fold(fun(K,V,A) -> create_ics_body(K,V,A) end, <<"">>, Modified).

get_recipient(ICSbody) ->
    #{freebusy := #{attendee := Attendee}} = eics:decode(ICSbody),
    lists:flatten(lists:nth(4, Attendee)).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Gets the local time and formats it so that it can be given as DTSTAMP
get_local_time() ->
    {Day, Hour} = calendar:local_time(),
    {LDay, LHour} = {lists:map(fun(L) -> integer_to_list(L) end, tuple_to_list(Day)),
                     lists:map(fun(L) -> integer_to_list(L) end, tuple_to_list(Hour))},
    [LDay, 84 ,LHour, "Z"].

%% @doc Collects parsed event datas of the given user
find_user_events(User) ->
    Events = ecalendar_db:get_user_list(User),
    find_user_events(Events, User, []).

find_user_events([], _, Acc) ->
    Acc;

find_user_events([Current | Rest], User, Acc) ->
    {_, [_, _, _, ParsedEventData]} = Current,
    #{events := Event} = ParsedEventData,
    find_user_events(Rest, User, lists:merge(Event, Acc)).

%% @doc Gets the username from the ATTENDEE ICS element
get_username(FreebusyData) ->
    Part = lists:nth(3, lists:nth(4, FreebusyData)),
    Bin = binary:list_to_bin(Part),
    lists:nth(1, binary:split(Bin, <<"@">>)).

%% @doc Returns the start and end time of the input event
get_start_end_time(ParsedICS, Place) ->
    #{dtstart := Start, dtend := End} = ParsedICS,
    case (is_list(lists:nth(2, Start))) andalso lists:flatten(lists:nth(2, Start)) =:= ";VALUE=DATE" of
        true ->
            create_whole_day_list(lists:nth(4, Start), lists:nth(4, End));
        false ->
            {lists:nth(Place, Start), lists:nth(Place, End)}
    end.

%% @doc Creates start/end times for whole day events
create_whole_day_list(InStart, InEnd) ->
    {[InStart, 84, ["00", "00", "00"]],
     [InEnd, 84, ["00", "0", "00"]]}.

%% @doc Iterates through the events and collects the times that are in the asked time interval
create_availability([], _, Acc) ->
    Acc;

create_availability([{Start1, End1} | Rest], {Start2, End2}, Acc) ->
    ToInsert = case (End1 < Start2) or (End2 < Start1) of
                   false ->
                       {LStart, LEnd} = {lists:flatten(Start1), lists:flatten(End1)},
                       create_line(binary:list_to_bin(LStart), binary:list_to_bin(LEnd));
                   true ->
                       <<"">>
               end,
    create_availability(Rest, {Start2, End2}, <<Acc/binary, ToInsert/binary>>).

%% @doc Creates a FREEBUSY ICS element line
create_line(Start, End) ->
    <<"FREEBUSY;FBTYPE=BUSY:", Start/binary, "Z/", End/binary, "Z\r\n">>.

%% @doc Creates the ICS body from the map
create_ics_body(Key, Value, Acc) when is_map(Value) ->
    #{type := Type} = Value,
    Component = binary:list_to_bin(string:to_upper(atom_to_list(Type))),
    BeginComp = <<"BEGIN:V", Component/binary, "\r\n">>,
    FullComp = maps:fold(fun(K,V,A) -> create_ics_body(K,V,A) end, BeginComp, Value),
    <<Acc/binary, FullComp/binary, "END:V", Component/binary, "\r\n">>;

create_ics_body(Key, Value, Acc) when is_list(Value) ->
    This = binary:list_to_bin(lists:flatten(Value)),
    <<Acc/binary, This/binary>>;

create_ics_body(_, _, Acc) ->
    Acc.
