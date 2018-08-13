%%%-------------------------------------------------------------------
%%% @doc ecalendar public API for database management
%%%-------------------------------------------------------------------

-module(ecalendar_db).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start/0,
         create_user/2,
         delete_user/1,
         authenticate_user/2,
         delete_event/2,
         insert_event_into_db/2,
         event_exists/1,
         get_value/1,
         get_user_list/1,
         user_exists/1
        ]).

%%====================================================================
%% API
%%====================================================================

start() ->
    ok = ecalendar_db_credential:start(),
    ok = ecalendar_db_calendar:start(),
    ok.

%% @doc Create a new user.
-spec create_user(Username :: binary(), Password :: binary()) -> {ok | error, binary()}.
create_user(Username, Password) ->
    case ecalendar_db_credential:add(Username, Password) of
        {ok, _} ->
            ecalendar_db_calendar:add_new_user_calendar(Username),
            {ok, <<Username/binary, " user created">>};
        _ ->
            {error, "Error"}
    end.

%% @doc Delete a user.
-spec delete_user(Username :: binary()) -> {ok, deleted} | {error, any}.
delete_user(Username) ->
    case ecalendar_db_calendar:delete_user_calendar(Username) of
        {ok, _} ->
            ecalendar_db_credential:delete(Username);
        _ ->
            {error, "Error"}
    end.

%% @doc Check for a user with the specified credentials.
-spec authenticate_user(Username :: binary(), Password :: binary()) -> true | false.
authenticate_user(Username, Password) ->
    case ecalendar_db_credential:find(Username) of
        [Username, Password] ->
            true;
        [Username | _] ->
            false;
        [] ->
            false
    end.

%% @doc Delete the event of a user.
-spec delete_event(Username :: binary(), Eventname :: binary()) -> ok.
delete_event(Username, Eventname) ->
    ecalendar_db_calendar:delete_data(Username, Eventname).

%% @doc Add a new event to the database.
-spec insert_event_into_db(Key :: binary(), Value :: [binary()]) -> ok.
insert_event_into_db(Key, Value) ->
    ecalendar_db_calendar:add_component(Key, Value).

%% @doc Check if an event exists or not.
-spec event_exists(Key :: binary()) -> true | false.
event_exists(Event) ->
    ecalendar_db_calendar:is_exists(Event).

%% @doc Get the specified event.
-spec get_value(Key :: binary()) -> CalendarList :: {Data :: binary(), Etag :: binary(), URI :: binary(), User :: binary()}.
get_value(Key) ->
    ecalendar_db_calendar:get_component(Key).

%% @doc Get the events of a user as a list.
-spec get_user_list(Username :: binary()) -> [{Filename :: binary(), [binary()]}].
get_user_list(Username) ->
    ecalendar_db_calendar:get_user_components(Username).

%% @doc Check if the user exists or not.
-spec user_exists(Username :: binary()) -> true | false.
user_exists(Username) ->
    ecalendar_db_credential:user_exists(Username).
