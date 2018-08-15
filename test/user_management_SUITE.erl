-module(user_management_SUITE).
-include("ecalendar_test.hrl").

-compile(export_all).

all() -> [create_user,
          create_existing_user,
          delete_user,
          delete_not_existing_user
         ].

%%------------------------------------------------------------------------------
%% SUITE init/end
%%------------------------------------------------------------------------------

init_per_suite(Config) ->
    application:ensure_all_started(ecalendar),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(ecalendar),
    ok.

%%------------------------------------------------------------------------------
%% TESTCASE init/end
%%------------------------------------------------------------------------------

init_per_testcase(delete_user, Config) ->
    {ok, 'user_created'} = ecalendar_db:create_user(<<"testuser">>, <<"password">>),
    init_per_testcase(common, Config);

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config1) ->
    ok = ecalendar_db:drop(),
    ok.

%%------------------------------------------------------------------------------
%% TESTCASES
%%------------------------------------------------------------------------------

%% @doc Create a not existing user.
create_user(Config) ->
    ?assertEqual(false, ecalendar_db:user_exists(<<"testuser">>)),
    ?assertEqual({ok, 'user_created'}, ecalendar_db:create_user(<<"testuser">>, <<"password">>)),
    ?assertEqual(true, ecalendar_db:user_exists(<<"testuser">>)),

    ok.

%% @doc Create an existing user.
create_existing_user(Config) ->
    ?assertEqual({ok, 'user_created'}, ecalendar_db:create_user(<<"testuser">>, <<"password">>)),
    ?assertEqual(true, ecalendar_db:user_exists(<<"testuser">>)),
    ?assertEqual({error, 'already_exist'}, ecalendar_db:create_user(<<"testuser">>, <<"password">>)),

    ok.

%% @doc Delete an existing user.
delete_user(Config) ->
    ?assertEqual(true, ecalendar_db:user_exists(<<"testuser">>)),
    ?assertEqual({ok, 'deleted'}, ecalendar_db:delete_user(<<"testuser">>)),
    ?assertEqual(false, ecalendar_db:user_exists(<<"testuser">>)),

    ok.

%% @doc Delete a not existing user.
delete_not_existing_user(Config) ->
    ?assertEqual(false, ecalendar_db:user_exists(<<"testuser">>)),
    ?assertEqual({error, 'missing_calendar'}, ecalendar_db:delete_user(<<"testuser">>)),

    ok.
