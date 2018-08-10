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
delete/1]).

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

user_exists(Username) ->
    ets:member(authorization, Username).

find(Username) ->
    ets:lookup(authorization, Username).

add(Username, Password) ->
    case find(Username) of
        [] ->
            PasswordEncoded = encode_credentials(Username, Password),
            ets:insert(authorization, {Username, PasswordEncoded}),
            {ok, OpenedFile} = file:open(filename:join([code:priv_dir(?APPLICATION), <<".htpasswd">>]), [append, binary]),
            file:write(OpenedFile, <<Username/binary, ":", PasswordEncoded/binary, "\n">>),
            {ok, added};
        _ ->
            {error, exists}
    end.

delete(Username) ->
    case find(Username) of
        [] ->
            {error, nexists};
        _ ->
            ets:delete(authorization, Username),
            save_auth_data_to_file(),
            {ok, deleted}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

save_auth_data_to_file() ->
    AuthData = create_auth_data(),
    Path = filename:join([code:priv_dir(?APPLICATION), <<".htpasswd">>]),
    file:delete(Path),
    {ok, OpenedFile} = file:open(Path, [write, read, binary]),
    file:write(OpenedFile, AuthData),
    file:close(OpenedFile).

create_auth_data() ->
    create_auth_data(ets:first(authorization), <<"">>).

create_auth_data('$end_of_table', Acc) ->
    Acc;

create_auth_data(Username, Acc) ->
    [{Username, PassHash}] = ets:lookup(authorization, Username),
    create_auth_data(ets:next(authorization, Username), <<Acc/binary, Username/binary, ":", PassHash/binary, "\n">>).


encode_credentials(Username, Password) ->
    code64:encode(<<Username/binary, ":", Password/binary>>).

get_htpasswd_path() ->
    filename:join([code:priv_dir(?APPLICATION), <<".htpasswd">>]).

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

parse_user_credential(Line) ->
    [User, Password] = binary:split(Line, <<":">>),
    {User, Password}.

save(Credentials) when is_list(Credentials) ->
    [ok = save(Credential) || Credential <- Credentials],
    ok;

save({User, Password}) ->
    ets:insert(authorization, {User, Password}),
    ok.
