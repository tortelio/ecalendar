%%%-------------------------------------------------------------------
%%% @doc Module for calendar user administration.
%%%-------------------------------------------------------------------

-module(ecalendar_user).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([exists/1,
         create/2,
         delete/1
        ]).

%%====================================================================
%% API
%%====================================================================

%% @doc Dcheck if the user exists(has an existing directory).
exists(Username) ->
    filelib:is_dir(<<"data/", Username/binary>>).

%% @doc Create a directory and an ets.
create(Username, Password) ->
    case exists(Username) of
        true ->
            io:format(<<Username/binary, " already exists~n">>),
            {error, <<Username/binary, " already exists">>};
        false ->
            filelib:ensure_dir(<<"data/", Username/binary, "/">>),
            {ok, OpenedFile} = file:open(<<"Auth">>, [append, binary]),
            file:write(OpenedFile, <<Username/binary, ":", Password/binary, "\n">>),
            ets:insert(authorization, {Username, Password}),
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
            ets:delete(authorization, Username),
            
            %% ets iteration to binary and delete+make new Auth.
            AuthData = create_auth_data(),
            file:delete(<<"Auth">>),
            {ok, OpenedFile} = file:open(<<"Auth">>, [write, read, binary]),
            file:write(OpenedFile, AuthData),
            file:close(OpenedFile),
            
            io:format(<<Username/binary, "  user deleted~n">>),
            {ok, <<Username/binary, "  deleted">>};
        false ->
            io:format(<<Username/binary, "  user does not exist~n">>),
            {error, <<Username/binary, "  does not exist">>}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

create_auth_data() ->
    create_auth_data(ets:first(authorization), <<"">>).

create_auth_data('$end_of_table', Acc) ->
    io:format(Acc),
    Acc;

create_auth_data(Username, Acc) ->
    [{Username, PassHash}] = ets:lookup(authorization, Username),
    create_auth_data(ets:next(authorization, Username), <<Acc/binary, Username/binary, ":", PassHash/binary, "\n">>).
