%%%-------------------------------------------------------------------
%%% @doc ecalendar_db_calendar public API
%%%-------------------------------------------------------------------

-module(ecalendar_db_calendar).
-include("ecalendar.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start/0,
         is_exists/1,
         add_new_user_calendar/1,
         add_component/2,
         get_component/1,
         get_user_components/1,
         delete_data/1,
         delete_user_calendar/1,
         delete_all/0,
         ics_time_to_utc/1]).

%%====================================================================
%% API
%%====================================================================

start() ->
    ets:new(calendar, [set, named_table, public]),
    ok = load(),

    ok.

%%====================================================================
%% Exported functions
%%====================================================================

%% @doc Check for a component in the ets.
-spec is_exists(Key :: binary()) -> true | false.
is_exists(Key) ->
    ets:member(calendar, Key).

%% @doc Return the component from the ets.
-spec get_component(Key :: binary()) -> CalendarList :: {Data :: binary(), Etag :: binary(), URI :: binary(), User :: binary()}.
get_component(Key) ->
    [{Key, CalendarList} | _] = ets:lookup(calendar, Key),
    CalendarList.

%% @doc Return all components of a user.
-spec get_user_components(User :: binary()) -> [{Filename :: binary(), [binary()]}].
get_user_components(User) ->
    ets:match_object(calendar, {'_', ['_', '_', User, '_']}).

%% @doc Add a new component to the database.
-spec add_component(Filename :: binary(), Value :: [binary()]) -> ok.
add_component(URI, Value) ->
    ets:insert(calendar,{URI, Value}),
    Username = lists:nth(3, Value),
    write_to_file(Username, URI).

%% @doc Create an empty calendar directory for the new user.
-spec add_new_user_calendar(Username :: binary()) -> ok.
add_new_user_calendar(Username) ->
    BaseDir = code:priv_dir(?APPLICATION),
    filelib:ensure_dir(filename:join([BaseDir, <<"data/">>, Username, <<"calendar/">>, <<"valami">>])),
    filelib:ensure_dir(filename:join([BaseDir, <<"data/">>, Username, <<"inbox/">>, <<"valami">>])),
    filelib:ensure_dir(filename:join([BaseDir, <<"data/">>, Username, <<"outbox/">>, <<"valami">>])).

%% @doc Delete the specified calendar component file.
-spec delete_data(Filename :: binary()) -> ok.
delete_data(URI) ->
    BaseDir = code:priv_dir(?APPLICATION),
    FullURI = filename:join([BaseDir, <<"data", URI/binary>>]),
    io:format("DELETING EVENT FILE~n"),
    io:format(FullURI),
    io:format("~n"),
    file:delete(FullURI),
    ets:delete(calendar, URI),
    io:format("EVENT DELETED~n").

%% @doc Delete the whole calendar of the specified user.
-spec delete_user_calendar(Username :: binary()) -> {ok | error, binary()}.
delete_user_calendar(Username) ->
    io:format("Deleting user~n"),
    BaseDir = code:priv_dir(?APPLICATION),
    case file:list_dir(filename:join([BaseDir, <<"data">>, Username, <<"calendar">>])) of
        {ok, Filenames} ->
            lists:foreach(fun(Filename1) ->
                                  io:format("...~n"),
                                  Filename2 = binary:list_to_bin(Filename1),
                                  delete_data(filename:join([Username, <<"calendar">>, Filename2]))
                          end, Filenames),
            file:del_dir(filename:join([BaseDir, <<"data">>, Username, <<"calendar">>])),
            file:del_dir(filename:join([BaseDir, <<"data">>, Username])),
            io:format(<<Username/binary, " user deleted~n">>),
            {ok, 'deleted'};
        {error, _} ->
            {error, 'missing_calendar'}
    end.

delete_all() ->
    BaseDir = code:priv_dir(?APPLICATION),
    case file:list_dir(filename:join([BaseDir, <<"data">>])) of
        {ok, UsersDirs} ->
            lists:foreach(fun(UserDir) ->
                                  case file:list_dir(filename:join([BaseDir, <<"data">>, UserDir, <<"calendar">>])) of
                                      {ok, UserEvents} ->
                                          lists:foreach(fun(UserEvent) ->
                                                                io:format(UserEvent),
                                                                delete_data(filename:join([<<"/">>, UserDir, <<"calendar">>, UserEvent]))
                                                        end, UserEvents),
                                          file:del_dir(filename:join([BaseDir, <<"data">>, UserDir, <<"calendar">>])),
                                          file:del_dir(filename:join([BaseDir, <<"data">>, UserDir])),
                                          {error, <<"Calendar does not exist">>}
                                  end
                          end, UsersDirs),
            {ok, <<"SERVER HAS BEEN WIPED">>};
        {error, _} ->
            {error, <<"THERE IS NO SERVER DATA">>}
    end.

ics_time_to_utc(ParsedBody) ->
    #{events := Eventlist} = ParsedBody,
    Event = lists:nth(1, Eventlist),
    #{dtstart := Start, dtend := End} = Event,
    TempDtStart = iso8601:parse(list_to_binary(lists:nth(4, Start))),
    TempDtEnd = iso8601:parse(list_to_binary(lists:nth(4, End))),
    #{timezone := TimeZones} = ParsedBody,
    case maps:is_key(daylight, TimeZones) of
        true ->
            #{daylight := DayLight} = TimeZones,
            #{rrule := RRD} = DayLight,
            #{standard := StanDard} = TimeZones,
            #{rrule := RRS} = StanDard,
            #{tzoffsetto := TOSD} = DayLight,
            #{tzoffsetto := TOSS} = StanDard,
            Tosd = string:chomp(lists:nth(2, binary:split(list_to_binary(TOSD), <<":">>))),
            TosdHour = list_to_integer(binary_to_list(string:slice(Tosd, 0, 3))),
            TosdMin = list_to_integer(binary_to_list(string:slice(Tosd, 3, 5))),
            Toss = string:chomp(lists:nth(2, binary:split(list_to_binary(TOSS), <<":">>))),
            TossHour = list_to_integer(binary_to_list(string:slice(Toss, 0, 3))),
            TossMin = list_to_integer(binary_to_list(string:slice(Toss, 3, 5))),
            case (TempDtStart < time_rrule_date(RRS, TempDtStart)) and (TempDtStart > time_rrule_date(RRD, TempDtStart)) of 
                true ->
                    DtStart = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(TempDtStart) + TosdHour*3600 + TosdMin*60);
                false ->
                    DtStart = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(TempDtStart) + TossHour*3600 + TossMin*60)
            end,
            case (TempDtEnd < time_rrule_date(RRS, TempDtEnd)) and (TempDtEnd > time_rrule_date(RRD, TempDtEnd)) of 
                true ->
                    DtEnd = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(TempDtEnd) + TosdHour*3600 + TosdMin*60);
                false ->
                    DtEnd = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(TempDtEnd) + TossHour*3600 + TossMin*60)
            end;
        false ->
            #{standard := StanDard} = TimeZones,
            #{tzoffsetto := TOSD} = StanDard,
            Tosd = string:chomp(lists:nth(2, binary:split(list_to_binary(TOSD), <<":">>))),
            TosdHour = list_to_integer(binary_to_list(string:slice(Tosd, 0, 3))),
            TosdMin = list_to_integer(binary_to_list(string:slice(Tosd, 3, 5))),
            DtStart = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(TempDtStart) + TosdHour*3600 + TosdMin*60),
            DtEnd = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(TempDtEnd) + TosdHour*3600 + TosdMin*60)
    end,
    {DtStart, DtEnd}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Load the saved ecalendar data into the calendar ets.
load() ->
    BaseDir = code:priv_dir(?APPLICATION),
    filelib:ensure_dir(filename:join([BaseDir, <<"data/valami">>])),
    Path = filename:join([BaseDir, <<"data/">>]),
    filelib:ensure_dir(Path),
    io:format("~nLOADING SAVED DATA~n"),
    {ok, UsersDirs} = file:list_dir(Path),
    io:format("All users: "),
    io:format(UsersDirs),
    io:format("~n"),

    ok = load_calendars([filename:join([Path, D, <<"calendar/">>]) || D <- UsersDirs]),

    io:format("LOADING FINISHED~n").

%% @doc Recursive function that loads the calendars of the users into the calendar ets.
load_calendars([]) ->
    ok;

load_calendars([Directory | Directories]) ->
    ok = load_calendar(Directory),
    load_calendars(Directories).

load_calendar(Directory) ->
    {ok, Filenames} = file:list_dir(Directory),

    Username = filename:basename(filename:dirname(Directory)),

    io:format(<<Username/binary, "'s calendar is loading...">>),
    ok = load_files(Username, [filename:join([Directory, Fn]) || Fn <- Filenames]),
    io:format("Calendar loading finished~n~n"),
    ok.

%% @doc Recursive function that loads the events of a user into the calendar ets.
-spec load_files(binary(), [string() | binary()]) -> ok.
load_files(_, []) ->
    ok;

load_files(Username, [Path | Paths]) ->
    ok = load_file(Username, Path),
    load_files(Username, Paths).

%% @doc Load the event of a user into the calendar ets.
-spec load_file(Username :: binary(), string() | binary()) -> ok.
load_file(Username, Path) ->
    Filename = filename:basename(Path),
    %{ok, OpenedFile} = file:open(Path, [read, binary]),

    {ok, RawFile} = file:read_file(Path),
    SplitRawFile = binary:split(RawFile, <<"\r\n">>),
    Etag = lists:nth(1, SplitRawFile),
    Data = lists:nth(2, SplitRawFile),
    Uri = <<"/", Username/binary, "/calendar/", Filename/binary>>,
    ParsedBody = eics:decode(Data),
    %%
    %case maps:is_key(timezone, ParsedBody) of
        %true ->
            %{DtStart, DtEnd} = ics_time_to_utc(ParsedBody);
        %false ->
            %#{events := Eventlist} = ParsedBody,
            %Event = lists:nth(1, Eventlist),
            %#{dtstart := Start, dtend := End} = Event,
            %DtStart = iso8601:parse(list_to_binary(lists:nth(4, Start))),
            %DtEnd = iso8601:parse(list_to_binary(lists:nth(4, End)))
        %end,
    %%
    ets:insert(calendar, {Uri, [Data, Etag, Username, ParsedBody]}),
    ok.

time_rrule_date(RRule, Date)->
    {{Year, _, _},{_, _, _}} = Date,
    Month = list_to_integer(binary_to_list(string:chomp(lists:nth(2, binary:split(list_to_binary(RRule), <<";BYMONTH=">>))))),
    NumDay = lists:nth(2, binary:split(lists:nth(1, binary:split(list_to_binary(RRule), <<";BYMONTH=">>)), <<";BYDAY=">>)),
    Num = list_to_integer(binary_to_list(string:slice(NumDay, 0, 2))),
    DayType = string:slice(NumDay, 2),
    case DayType of 
    <<"MO">> ->
    DayTypeId = 1;
    <<"TU">> ->
    DayTypeId = 2;
    <<"WE">> ->
    DayTypeId = 3;
    <<"TH">> ->
    DayTypeId = 4;
    <<"FR">> ->
    DayTypeId = 5;
    <<"SA">> ->
    DayTypeId = 6;
    <<"SU">> ->
    DayTypeId = 7
    end,
    case (Num < 0) of
        true ->
            LastDayTypeId = calendar:day_of_the_week(Year, Month, calendar:last_day_of_the_month(Year, Month)),
            DayDiff = LastDayTypeId - DayTypeId,
            DayMod = DayDiff rem 7,
            FinalDayDiff =(7+DayMod) rem 7,
            Day = calendar:last_day_of_the_month(Year, Month) - FinalDayDiff + (Num+1)*7;
        false ->
            FirstDayTypeId = calendar:day_of_the_week(Year, Month, 1),
            DayDiff = DayTypeId - FirstDayTypeId,
            DayMod = DayDiff rem 7,
            FinalDayDiff =(7+DayMod) rem 7,
            Day = 1 + FinalDayDiff + (Num-1)*7
    end,
    {{Year, Month, Day}, {0, 0, 0}}.

%% @doc Write the calendar component into an ics file.
-spec write_to_file(User :: binary(), Key :: binary()) -> ok.
write_to_file(User, URI) ->
    io:format("SAVING EVENT TO FILE~n"),
    io:format(User),
    io:format("-----"),
    io:format(URI),
    io:format("~n"),
    BaseDir = code:priv_dir(?APPLICATION),
    {ok, OpenedFile} = file:open(filename:join([BaseDir, <<"data", URI/binary>>]), [write, read, binary]),
    [{URI, CalendarList} | _] = ets:lookup(calendar, URI),
    ComponentData = lists:nth(1, CalendarList),
    ComponentEtag = lists:nth(2, CalendarList),
    file:write(OpenedFile, <<ComponentEtag/binary, "\r\n", ComponentData/binary>>),
    file:close(OpenedFile),
    io:format("EVENT SAVED~n").
