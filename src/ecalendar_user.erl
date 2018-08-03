%%%-------------------------------------------------------------------
%%% @doc Module for calendar user administration.
%%%-------------------------------------------------------------------

-module(ecalendar_user).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([exists/1,
create/1,
delete/1
]).

%%====================================================================
%% API
%%====================================================================

%% @doc Dcheck if the user exists(has an existing directory).
exists(Username) ->
filelib:is_dir(<<"data/", Username/binary>>).

%% @doc Create a directory and an ets for the user.
create(Username) ->
case exists(Username) of
true ->
io:format(<<Username/binary, "  already exists~n">>),
{error, <<Username/binary, " already exists">>};
false ->
io:format(<<Username/binary, "  user created~n">>),
filelib:ensure_dir(<<"data/", Username/binary, "/">>),
ets:new(binary_to_atom(Username, utf8), [set, named_table, public]),
{ok, <<Username/binary, "  user created">>}
end.

%% @doc Delete the user's directory and ets.
delete(Username) ->
case exists(Username) of
true ->
%% TODO: make os free deletion of a not empty dir.
DirPath = binary_to_list(<<"data/", Username/binary>>),
os:cmd("rm -Rf " ++ DirPath),
%del_dir(<<"data/", Username/binary>>),
%------------------------------------------------
ets:delete(binary_to_atom(Username, utf8)),
io:format(<<Username/binary, "  user deleted~n">>),
{ok, <<Username/binary, "  deleted">>};
false ->
io:format(<<Username/binary, "  user does not exist~n">>),
{error, <<Username/binary, "  does not exist">>}
end.



%%====================================================================
%% Internal functions
%%====================================================================

%% UNUSED/NOT WORKING
del_dir(Dir) ->
   lists:foreach(fun(D) ->
                    ok = file:del_dir(D)
                 end, del_all_files([Dir], [])).

del_all_files([], EmptyDirs) ->
   EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
   {ok, FilesInDir} = file:list_dir(Dir),
   {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                  Path = <<Dir/binary, "/", F/binary>>,
                                  case filelib:is_dir(Path) of
                                     true ->
                                          {Fs, [Path | Ds]};
                                     false ->
                                          {[Path | Fs], Ds}
                                  end
                               end, {[],[]}, FilesInDir),
   lists:foreach(fun(F) ->
                         ok = file:delete(F)
                 end, Files),
   del_all_files(T ++ Dirs, [Dir | EmptyDirs]).
