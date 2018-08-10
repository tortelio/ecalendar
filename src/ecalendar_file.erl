%%%-------------------------------------------------------------------
%%% @doc ecalendar public API
%%%-------------------------------------------------------------------

-module(ecalendar_file).
-include("ecalendar.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([save_ets_data/1,
         write_to_file/2,
         delete_file/2,
         load_calendar_data/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Save the ets into a file.
save_ets_data(Cal) ->
    io:format("SAVING DATA~n"),
    save_ets_data(Cal, ets:first(Cal)).

%% @doc Write the calendar component into an ics file.
write_to_file(Cal, Key) ->
    io:format("SAVING EVENT TO FILE~n"),
    {ok, OpenedFile} = file:open(<<"data/", Cal/binary, "/", Key/binary>>, [write, read, binary]),
    [{Key, CalendarList} | _] = ets:lookup(calendar, Key),
    ComponentData = lists:nth(1, CalendarList),
    ComponentEtag = lists:nth(2, CalendarList),
    file:write(OpenedFile, <<ComponentEtag/binary, "\n", ComponentData/binary>>),
    file:close(OpenedFile),
    io:format("EVENT SAVED~n").

%% @doc Delete the specified calendar component file.
delete_file(Username, Filename) ->
    io:format("DELETING EVENT FILE~n"),
    io:format(Username),
    io:format("--------"),
    io:format(Filename),
    io:format("~n"),
    file:delete(<<"data/", Username/binary, "/", Filename/binary>>),
    io:format("EVENT DELETED~n"),
    ets:i().

%% @doc Load the stored calendar data into one ets
load_calendar_data() ->
    filelib:ensure_dir("data/"),
    io:format("~nLOADING SAVED DATA~n"),
    {ok, UsersDirs} = file:list_dir("data/"),
    io:format("All users: "),
    io:format(UsersDirs),
    io:format("~n"),

    BaseDir = code:priv_dir(?APPLICATION),
    ok = load_calendars([filename:join([BaseDir, D]) || D <- UsersDirs]),

    io:format("LOADING FINISHED~n").

load_calendars([]) ->
    ok;
load_calendars([Directory | Directories]) ->
    ok = load_calendar(Directory),
    load_calendars(Directories).

load_calendar(Directory) ->
    {ok, Filenames} = file:list_dir(Directory),

    Username = filename:basename(Directory),

    ok = load_files(Username, [filename:join([Directory, Fn]) || Fn <- Filenames]),
    ok.

load_files(_, []) ->
    ok;
load_files(Username, [Path | Paths]) ->
    ok = load_file(Username, Path),
    load_files(Username, Paths).

load_file(Username, Path) ->
    Filename = filename:basename(Path),
    {ok, OpenedFile} = file:open(Path, [read, binary]),

    {ok, Etag1} = file:read_line(OpenedFile),

    Data = read_rest(OpenedFile, file:read_line(OpenedFile), <<"">>),

    Uri = <<"/", Username/binary, "/calendar/", Filename/binary>>,

    Etag2 = string:tokens(erlang:binary_to_list(Etag1), "\n"),
    Etag = list_to_binary(Etag2),
    ets:insert(calendar, {Filename, [Data, Etag, Uri, Username]}),

    ok.

%%====================================================================
%% Internal functions
%%====================================================================

save_ets_data(_, '$end_of_table') ->
    io:format("END OF CALENDAR~n");

save_ets_data(Cal, Key) ->
    io:format("...~n"),
    write_to_file(Cal, Key),
    save_ets_data(Cal, ets:next(Cal, Key)).

read_rest(OpenedFile, CurrentLine, Acc) ->
    case CurrentLine of
        eof ->
            file:close(OpenedFile),
            Acc;
        {ok, Data} ->
            read_rest(OpenedFile, file:read_line(OpenedFile), <<Acc/binary, Data/binary>>)
    end.
