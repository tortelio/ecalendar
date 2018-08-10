%%%-------------------------------------------------------------------
%%% @doc ecalendar_db_credential public API
%%%-------------------------------------------------------------------

-module(ecalendar_db_credential).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    ets:new(authorization, [set, named_table, public]),

    % Load credentials from htpasswd file
    ok = load(get_htpasswd_path()),

    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_htpasswd_path() ->
    filename:join([code:priv_dir(?APPLICATION), <<".htpasswd">>]).

load(Path) ->
    case file:open(Path, [read, write, binary]) of
        {ok, FD} ->
            Credentials = read_user_credentials(FD),
            ok = save(Credentials),
            ok = file:close(FD),
            Credentials;
        {error, Reason} ->
            {error, Reason}
    end.

read_user_credentials(FD) ->
    read_user_credentials(FD, []).

read_user_credantials(FD, Credential) ->
    case file:readline(FD) of
        eof ->
            Credentials;
        Line ->
            Credential = parse_user_credential(Line),
            read_user_credentials(FD, [Credential | Credentials])
    end.

parse_user_credential(Line) ->
    [User, Password] = binary:split(Data, <<":">>),
    {User, Password}.

save(Credentials) when is_list(Credentials) ->
    [ok = save(Credential) || Credential <- Credentials],
    ok;

save({User, Password}) ->
    ets:insert(authorization, {User, Password}),
    ok.
