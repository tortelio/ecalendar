%%%-------------------------------------------------------------------
%%% @doc ecalendar public API
%%%-------------------------------------------------------------------

-module(ecalendar_file).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([save_ets_data/1,
write_to_file/2,
delete_file/2,
load_ets_data/0]).

%%====================================================================
%% API
%%====================================================================
save_ets_data(Cal) ->
    io:format("SAVING DATA~n"),
save_ets_data(Cal, ets:first(Cal)).

save_ets_data(_, '$end_of_table') ->
    io:format("END OF CALENDAR~n");
    
save_ets_data(Cal, Key) ->
    io:format("...~n"),
write_to_file(Cal, Key),
    save_ets_data(Cal, ets:next(Cal, Key)).
    
write_to_file(Cal, Key) ->
    io:format("SAVING EVENT TO FILE~n"),
{ok, OpenedFile} = file:open(<<"data/", Cal/binary, "/", Key/binary>>, [write, binary]),
    [{Key, CalendarList} | _] = ets:lookup(binary_to_atom(Cal, utf8), Key),
    ComponentData = lists:nth(1, CalendarList),
    ComponentEtag = lists:nth(2, CalendarList),
    file:write(OpenedFile, <<ComponentEtag/binary, "\n", ComponentData/binary>>),
    file:close(OpenedFile),
    io:format("EVENT SAVED~n").
    
delete_file(Username, Filename) ->
    io:format("DELETING EVENT FILE~n"),
    io:format(Username),
    io:format("--------"),
    io:format(Filename),
    io:format("~n"),
file:delete(<<"data/", Username/binary, "/", Filename/binary>>),
    io:format("EVENT DELETED~n").

load_ets_data() ->
io:format("LOADING SAVED DATA~n"),
{ok, UsersDirs} = file:list_dir("data/"),
lists:foreach(fun(Dirname1) ->
Dirname2 = binary:list_to_bin(Dirname1),
Dirname = binary_to_atom(Dirname2, utf8),
io:format("Creating new calendar~n"),
ets:new(Dirname, [set, named_table, public]),
{ok, Filenames} = file:list_dir(<<"data/", Dirname2/binary, "/">>),
lists:foreach(fun(Filename1) ->
io:format("...~n"),
Filename = binary:list_to_bin(Filename1),
{ok, OpenedFile} = file:open(<<"data/", Dirname2/binary, "/", Filename/binary>>, [read, binary]),
{ok, Etag1} = file:read_line(OpenedFile),
Data = read_rest(OpenedFile, file:read_line(OpenedFile), <<"">>),
DirnameBin = atom_to_binary(Dirname, utf8),
Uri = <<"http://localhost:8080/", DirnameBin/binary, "/calendar/", Filename/binary>>,
Etag2 = string:tokens(erlang:binary_to_list(Etag1), "\n"),
Etag = list_to_binary(Etag2),
ets:insert(Dirname, {Filename, [Data, Etag, Uri]})
end, Filenames)
end, UsersDirs),
io:format("LOADING FINISHED~n").

read_rest(OpenedFile, CurrentLine, Acc) ->
case CurrentLine of
eof ->
Acc;
{ok, Data} ->
read_rest(OpenedFile, file:read_line(OpenedFile), <<Acc/binary, Data/binary>>)
    end.
