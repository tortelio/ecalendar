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
         add/2,
         delete/1,
         delete_all/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    ets:new(authorization, [set, named_table, public]),

    % Load credentials from htpasswd file
    ok = load(get_htpasswd_path()),

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
        [{Username, EncodedPW}] ->
            binary:split(base64:decode(EncodedPW), <<":">>);
        _ ->
            []
    end.

%% @doc If username does not exists yet, add a new user to the authorization.
-spec add(Username :: binary(), Password :: binary()) -> {ok, added} | {error, exists}.
add(Username, Password) ->
    case find(Username) of
        [] ->
            PasswordEncoded = encode_credentials(Username, Password),
            ets:insert(authorization, {Username, PasswordEncoded}),
            {ok, OpenedFile} = file:open(filename:join([code:priv_dir(?APPLICATION), <<".htpasswd">>]), [append, binary]),
            file:write(OpenedFile, <<Username/binary, ":", PasswordEncoded/binary, "\n">>),
            {ok, added};
        _ ->
            {error, 'already_exist'}
    end.

%% @doc Delete a user's authorization data.
-spec delete(Username :: binary()) -> {ok, deleted} | {error, nexists}.
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
    file:close(OpenedFile).

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
    file:close(OpenedFile).

%% @doc Recursive function for authorization data saving.
-spec create_auth_data() -> Acc :: binary().
create_auth_data() ->
    create_auth_data(ets:first(authorization), <<"">>).

create_auth_data('$end_of_table', Acc) ->
    Acc;

create_auth_data(Username, Acc) ->
    [{Username, PassHash}] = ets:lookup(authorization, Username),
    create_auth_data(ets:next(authorization, Username), <<Acc/binary, Username/binary, ":", PassHash/binary, "\n">>).

%% @doc Encode the uncoded password in base64.
-spec encode_credentials(Username :: binary(), Password :: binary()) -> binary().
encode_credentials(Username, Password) ->
    base64:encode(<<Username/binary, ":", Password/binary>>).

%% @doc Return the full path for the .htpasswd file.
-spec get_htpasswd_path() -> string() | binary().
get_htpasswd_path() ->
    filename:join([code:priv_dir(?APPLICATION), <<".htpasswd">>]).

%% @doc Load the saved authorization data into the authorization ets.
-spec load(Path :: string() | binary()) -> {ok, FD :: pid()} | {error, Reason :: any()}.
load(Path) ->
    case file:open(Path, [read, write, binary]) of
        {ok, FD} ->
            Credentials = read_user_credentials(FD),
            ok = save(Credentials),
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
    [User, Password] = binary:split(Line3, <<":">>),
    {User, Password}.

%% @doc Save the authorization credentials into the authorization ets.
-spec save(Credentials :: list()) -> ok.
save(Credentials) when is_list(Credentials) ->
    [ok = save(Credential) || Credential <- Credentials],
    ok;

save({User, Password}) ->
    ets:insert(authorization, {User, Password}),
    ok.
