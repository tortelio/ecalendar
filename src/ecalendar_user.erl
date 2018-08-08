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

%% @doc Create a directory and an ets.
create(Username) ->
    case exists(Username) of
        true ->
            io:format(<<Username/binary, " already exists~n">>),
            {error, <<Username/binary, " already exists">>};
        false ->
            filelib:ensure_dir(<<"data/", Username/binary, "/">>),
            %ets:new(binary_to_atom(Username, utf8), [set, named_table, public]),
            io:format("~n"),
            io:format(<<Username/binary, " user created~n">>),
            {ok, <<Username/binary, " user created">>}
    end.

%% @doc Delete the user's directory and ets objects.
delete(Username) ->
    case exists(Username) of
        true ->
            io:format("Deleting user~n"),
            {ok, Filenames} = file:list_dir(<<"data/", Username/binary, "/">>),
            lists:foreach(fun(Filename1) ->
                                  io:format("...~n"),
                                  Filename2 = binary:list_to_bin(Filename1),
                                  file:delete(<<"data/", Username/binary, "/", Filename2/binary>>),
                                  ets:delete(calendar, Filename2)
                          end, Filenames),
            file:del_dir(<<"data/", Username/binary>>),
            %ets:delete(binary_to_atom(Username, utf8)),
            io:format(<<Username/binary, "  user deleted~n">>),
            {ok, <<Username/binary, "  deleted">>};
        false ->
            io:format(<<Username/binary, "  user does not exist~n">>),
            {error, <<Username/binary, "  does not exist">>}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
