%%%-------------------------------------------------------------------
%%% @doc ecalendar_db_credential public API
%%%-------------------------------------------------------------------

-module(ecalendar_db_credential).

-include("ecalendar.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start/0,
         user_exists/1,
         find/1,
         get_user_email/1,
         get_username_by_email/1,
         add/3,
         delete/1,
         delete_all/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    ets:new(authorization, [set, named_table, public]),

    % Load credentials from htpasswd file
    ok = load_credentials(get_file_path(<<".htpasswd">>)),
    ok = load_address(get_file_path(<<".address">>)),

    ok.

%%====================================================================
%% Exported functions
%%====================================================================

%% @doc Check for user in the authorization ets.
-spec user_exists(Username :: binary()) -> true | false.
user_exists(Username) ->
    ets:member(authorization, Username).

%% @doc Return a user's stored authorization data.
-spec find(Username :: binary()) -> [binary()].
find(Username) ->
    case ets:lookup(authorization, Username) of
        [{Username, {EncodedPW, _}}] ->
            binary:split(base64:decode(EncodedPW), <<":">>);
        _ ->
            []
    end.

get_user_email(Username) ->
    case ets:lookup(authorization, Username) of
        [{Username, {_, Email}}] ->
            Email;
        _ ->
            []
    end.

get_username_by_email(Email) ->
    [{Username, {Password, Email}}] = ets:match_object(authorization, {'_', {'_', Email}}),
    Username.


%% @doc If username does not exists yet, add a new user to the authorization.
-spec add(Username :: binary(), Password :: binary(), Email :: binary()) -> {ok, added} | {error, 'already_exist'}.
add(Username, Password, Email) ->
    case find(Username) of
        [] ->
            PasswordEncoded = encode_credentials(Username, Password),
            {ok, OpenedFile} = file:open(filename:join([code:priv_dir(?APPLICATION), <<".htpasswd">>]), [append, binary]),
            file:write(OpenedFile, <<Username/binary, ":", PasswordEncoded/binary, "\n">>),
            file:close(OpenedFile),
            {ok, OpenedFile2} = file:open(filename:join([code:priv_dir(?APPLICATION), <<".address">>]), [append, binary]),
            file:write(OpenedFile2, <<Username/binary, ":", Email/binary, "\n">>),
            file:close(OpenedFile2),
            ets:insert(authorization, {Username, {PasswordEncoded, Email}}),
            {ok, added};
        _ ->
            {error, 'already_exist'}
    end.

%% @doc Delete a user's authorization data.
-spec delete(Username :: binary()) -> {ok, deleted} | {error, 'missing_user'}.
delete(Username) ->
    case find(Username) of
        [] ->
            {error, 'missing_user'};
        _ ->
            ets:delete(authorization, Username),
            save_auth_data_to_file(),
            {ok, deleted}
    end.

%% @doc Delete all authorization data.
delete_all() ->
    ets:delete_all_objects(authorization),
    Path = filename:join([code:priv_dir(?APPLICATION), <<".htpasswd">>]),
    file:delete(Path),
    {ok, OpenedFile} = file:open(Path, [write, read, binary]),
    file:close(OpenedFile),
    Path2 = filename:join([code:priv_dir(?APPLICATION), <<".address">>]),
    file:delete(Path2),
    {ok, OpenedFile2} = file:open(Path2, [write, read, binary]),
    file:close(OpenedFile2).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Save the authorization ets data into a file.
-spec save_auth_data_to_file() -> ok.
save_auth_data_to_file() ->
    AuthData = create_auth_data(),
    Path = filename:join([code:priv_dir(?APPLICATION), <<".htpasswd">>]),
    file:delete(Path),
    {ok, OpenedFile} = file:open(Path, [write, read, binary]),
    file:write(OpenedFile, AuthData),
    file:close(OpenedFile),
    AdressData = create_address_data(),
    Path2 = filename:join([code:priv_dir(?APPLICATION), <<".address">>]),
    file:delete(Path2),
    {ok, OpenedFile2} = file:open(Path2, [write, read, binary]),
    file:write(OpenedFile2, AdressData),
    file:close(OpenedFile2).

%% @doc Recursive function for authorization data saving.
-spec create_address_data() -> Acc :: binary().
create_address_data() ->
    create_address_data(ets:first(authorization), <<"">>).

create_address_data('$end_of_table', Acc) ->
    Acc;

create_address_data(Username, Acc) ->
    [{Username, {_, Email}}] = ets:lookup(authorization, Username),
    create_auth_data(ets:next(authorization, Username), <<Acc/binary, Username/binary, ":", Email/binary, "\n">>).

%% @doc Recursive function for authorization data saving.
-spec create_auth_data() -> Acc :: binary().
create_auth_data() ->
    create_auth_data(ets:first(authorization), <<"">>).

create_auth_data('$end_of_table', Acc) ->
    Acc;

create_auth_data(Username, Acc) ->
    [{Username, {PassHash, _}}] = ets:lookup(authorization, Username),
    create_auth_data(ets:next(authorization, Username), <<Acc/binary, Username/binary, ":", PassHash/binary, "\n">>).

%% @doc Encode the uncoded password in base64.
-spec encode_credentials(Username :: binary(), Password :: binary()) -> binary().
encode_credentials(Username, Password) ->
    base64:encode(<<Username/binary, ":", Password/binary>>).

%% @doc Return the full path for the .htpasswd file.
-spec get_file_path(File :: binary()) -> string() | binary().
get_file_path(File) ->
    filename:join([code:priv_dir(?APPLICATION), File]).

%% @doc Load the saved authorization data into the authorization ets.
-spec load_credentials(Path :: string() | binary()) -> ok | {error, Reason :: any()}.
load_credentials(Path) ->
    case file:open(Path, [read, write, binary]) of
        {ok, FD} ->
            Credentials = read_user_credentials(FD),
            ok = save(Credentials),
            ok = file:close(FD),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Load the saved authorization data into the authorization ets.
-spec load_address(Path :: string() | binary()) -> ok | {error, Reason :: any()}.
load_address(Path) ->
    case file:open(Path, [read, write, binary]) of
        {ok, FD} ->
            Credentials = read_user_credentials(FD),
            ok = save_address(Credentials),
            ok = file:close(FD),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Recursive function to load all the authoriztion data.
-spec read_user_credentials(FD :: pid()) -> Credentials :: [].
read_user_credentials(FD) ->
    read_user_credentials(FD, []).

read_user_credentials(FD, Credentials) ->
    case file:read_line(FD) of
        eof ->
            Credentials;
        {ok, Line} ->
            Credential = parse_user_credential(Line),
            read_user_credentials(FD, [Credential | Credentials])
    end.

%% @doc Parse the saved authorization data for use.
-spec parse_user_credential(Line :: binary()) -> {User :: binary(), Password :: binary()}.
parse_user_credential(Line) ->
    Line2 = string:tokens(erlang:binary_to_list(Line), "\n"),
    Line3 = list_to_binary(Line2),
    [User, Data] = binary:split(Line3, <<":">>),
    {User, Data}.

%% @doc Save the authorization credentials into the authorization ets.
-spec save(Credentials :: list()) -> ok.
save(Credentials) when is_list(Credentials) ->
    [ok = save(Credential) || Credential <- Credentials],
    ok;

save({User, Password}) ->
    ets:insert(authorization, {User, {Password}}),
    ok.

%% @doc Save the authorization credentials into the authorization ets.
-spec save_address(Credentials :: list()) -> ok.
save_address(Credentials) when is_list(Credentials) ->
    [ok = save_address(Credential) || Credential <- Credentials],
    ok;

save_address({User, Email}) ->
    [{User, Password}] = ets:lookup(authorization, User),
    Data = erlang:insert_element(2, Password, Email),
    ets:insert(authorization, {User, Data}),
    ok.
