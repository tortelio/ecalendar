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
         add_component/2,
         get_component/1,
         get_user_components/1,
         delete_data/2]).

%%====================================================================
%% API
%%====================================================================

start() ->
    ets:new(calendar, [set, named_table, public]),

    % TODO move from ecalendar_file the corresponding parts here

    ok = load(),

    ok.

%%====================================================================
%% Exported functions
%%====================================================================

%% @doc Check for a component in the ets.
is_exists(Key) ->
    ets:member(calendar, Key).

%% @doc Return the component from the ets.
get_component(Key) ->
    [{Key, CalendarList} | _] = ets:lookup(calendar, Key),
    CalendarList.

%% @doc Return all components of a user.
get_user_components(User) ->
    ets:match_object(calendar, {'_', ['_', '_', '_', User]}).

%% @doc Add a new component to the database.
add_component(Filename, Value) ->
    ets:insert(calendar,{Filename, Value}),
    Username = lists:nth(4, Value),
    write_to_file(Username, Filename).

%% @doc Create an empty calendar directory for the new user.
add_new_user_calendar(Username) ->
    BaseDir = code:priv_dir(?APPLICATION),
    file:ensure_dir(filename:join([BaseDir, <<"data/">>, Username, <<"calendar/">>])).

%% @doc Delete the specified calendar component file.
delete_data(Username, Filename) ->
    BaseDir = code:priv_dir(?APPLICATION),
    io:format("DELETING EVENT FILE~n"),
    io:format(Username),
    io:format("--------"),
    io:format(Filename),
    io:format("~n"),
    file:delete(filename:join([BaseDir, <<"data">>, Username, <<"calendar">>, Filename])),
    ets:delete(calendar, Filename),
    io:format("EVENT DELETED~n").

%%====================================================================
%% Internal functions
%%====================================================================

load() ->
    BaseDir = code:priv_dir(?APPLICATION),
    Path = filename:join([BaseDir, <<"data/">>]),
    filelib:ensure_dir(Path),
    io:format("~nLOADING SAVED DATA~n"),
    {ok, UsersDirs} = file:list_dir(Path),
    io:format("All users: "),
    io:format(UsersDirs),
    io:format("~n"),

    ok = load_calendars([filename:join([Path, D, <<"calendar/">>]) || D <- UsersDirs]),

    io:format("LOADING FINISHED~n").

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
    io:format("~n"),
    io:format(Filename),
    io:format("---"),
    io:format(Etag),
    io:format("-----"),
    io:format(Uri),
    io:format("-----"),
    io:format(Username),
    io:format("~n"),
    ets:insert(calendar, {Filename, [Data, Etag, Uri, Username]}),

    ok.

read_rest(OpenedFile, CurrentLine, Acc) ->
    case CurrentLine of
        eof ->
            file:close(OpenedFile),
            Acc;
        {ok, Data} ->
            read_rest(OpenedFile, file:read_line(OpenedFile), <<Acc/binary, Data/binary>>)
    end.

%% @doc Write the calendar component into an ics file.
write_to_file(User, Key) ->
    io:format("SAVING EVENT TO FILE~n"),
    BaseDir = code:priv_dir(?APPLICATION),
    {ok, OpenedFile} = file:open(filename:join([BaseDir, <<"data">>, User, <<"calendar">>, Key]), [write, read, binary]),
    [{Key, CalendarList} | _] = ets:lookup(calendar, Key),
    ComponentData = lists:nth(1, CalendarList),
    ComponentEtag = lists:nth(2, CalendarList),
    file:write(OpenedFile, <<ComponentEtag/binary, "\n", ComponentData/binary>>),
    file:close(OpenedFile),
    io:format("EVENT SAVED~n").
