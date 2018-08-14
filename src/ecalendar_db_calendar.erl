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
         delete_data/2,
         delete_user_calendar/1]).

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
    ets:match_object(calendar, {'_', ['_', '_', '_', User, '_']}).

%% @doc Add a new component to the database.
-spec add_component(Filename :: binary(), Value :: [binary()]) -> ok.
add_component(Filename, Value) ->
    ets:insert(calendar,{Filename, Value}),
    Username = lists:nth(4, Value),
    write_to_file(Username, Filename).

%% @doc Create an empty calendar directory for the new user.
-spec add_new_user_calendar(Username :: binary()) -> ok.
add_new_user_calendar(Username) ->
    BaseDir = code:priv_dir(?APPLICATION),
    filelib:ensure_dir(filename:join([BaseDir, <<"data/">>, Username, <<"calendar/">>, <<"valami">>])).

%% @doc Delete the specified calendar component file.
-spec delete_data(Username :: binary(), Filename :: binary()) -> ok.
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
                              delete_data(Username, Filename2)
                          end, Filenames),
            file:del_dir(filename:join([BaseDir, <<"data">>, Username, <<"calendar">>])),
            file:del_dir(filename:join([BaseDir, <<"data">>, Username])),
            io:format(<<Username/binary, " user deleted~n">>),
            {ok, <<Username/binary, " deleted">>};
        {error, _} ->
            {error, <<"Calendar does not exist.">>}
        end.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Load the saved ecalendar data into the calendar ets.
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
    io:format("~n"),
    io:format(Filename),
    io:format("---"),
    io:format(Etag),
    io:format("-----"),
    io:format(Uri),
    io:format("-----"),
    io:format(Username),
    io:format("~n"),
    io:format(Data),
    ParsedBody = eics:decode(Data),
    ets:insert(calendar, {Filename, [Data, Etag, Uri, Username, ParsedBody]}),
    ok.

%% @doc Read the rest of an event file.
-spec read_rest(OpenedFile :: pid(), CurrentLine :: eof | {ok , Data :: binary()}, Acc :: binary()) -> Acc :: binary().
read_rest(OpenedFile, CurrentLine, Acc) ->
    case CurrentLine of
        eof ->
            file:close(OpenedFile),
            Acc;
        {ok, Data} ->
            read_rest(OpenedFile, file:read_line(OpenedFile), <<Acc/binary, Data/binary>>)
    end.

%% @doc Write the calendar component into an ics file.
-spec write_to_file(User :: binary(), Key :: binary()) -> ok.
write_to_file(User, Key) ->
    io:format("SAVING EVENT TO FILE~n"),
    BaseDir = code:priv_dir(?APPLICATION),
    {ok, OpenedFile} = file:open(filename:join([BaseDir, <<"data">>, User, <<"calendar">>, Key]), [write, read, binary]),
    [{Key, CalendarList} | _] = ets:lookup(calendar, Key),
    ComponentData = lists:nth(1, CalendarList),
    ComponentEtag = lists:nth(2, CalendarList),
    file:write(OpenedFile, <<ComponentEtag/binary, "\r\n", ComponentData/binary>>),
    file:close(OpenedFile),
    io:format("EVENT SAVED~n").
