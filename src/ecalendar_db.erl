%%%-------------------------------------------------------------------
%%% @doc ecalendar public API for database management
%%%-------------------------------------------------------------------

-module(ecalendar_db).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start/0,
         drop/0,
         create_user/2,
         delete_user/1,
         authenticate_user/2,
         delete_event/1,
         insert_event/2,
         event_exists/1,
         get_component/1,
         get_user_list/1,
         get_utc_time/1,
         user_exists/1
        ]).

%%====================================================================
%% API
%%====================================================================

start() ->
    ok = ecalendar_db_credential:start(),
    ok = ecalendar_db_calendar:start(),
    ok.

drop() ->
    ecalendar_db_calendar:delete_all(),
    ecalendar_db_credential:delete_all(),
    ok.

%% @doc Create a new user.
-spec create_user(Username :: binary(), Password :: binary()) -> {ok | error, binary()}.
create_user(Username, Password) ->
    case ecalendar_db_credential:add(Username, Password) of
        {ok, _} ->
            ecalendar_db_calendar:add_new_user_calendar(Username),
            {ok, 'user_created'};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Delete a user.
-spec delete_user(Username :: binary()) -> {ok, deleted} | {error, any}.
delete_user(Username) ->
    case ecalendar_db_calendar:delete_user_calendar(Username) of
        {ok, _} ->
            ecalendar_db_credential:delete(Username);
        {error, Reason} ->
            {error, Reason}
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
-spec delete_event(URI :: binary()) -> ok.
delete_event(URI) ->
    ecalendar_db_calendar:delete_data(URI).

%% @doc Add a new event to the database.
-spec insert_event(Key :: binary(), Value :: [binary()]) -> ok.
insert_event(URI, Value) ->
    ecalendar_db_calendar:add_component(URI, Value).

%% @doc Check if an event exists or not.
-spec event_exists(Key :: binary()) -> true | false.
event_exists(Event) ->
    ecalendar_db_calendar:is_exists(Event).

%% @doc Get the specified event.
-spec get_component(Key :: binary()) -> CalendarList :: {Data :: binary(), Etag :: binary(), URI :: binary(), User :: binary()}.
get_component(Key) ->
    ecalendar_db_calendar:get_component(Key).

%% @doc Get the events of a user as a list.
-spec get_user_list(Username :: binary()) -> [{Filename :: binary(), [binary()]}].
get_user_list(Username) ->
    ecalendar_db_calendar:get_user_components(Username).

get_utc_time(ParsedData) ->
    ecalendar_db_calendar:ics_time_to_utc(ParsedData).

%% @doc Check if the user exists or not.
-spec user_exists(Username :: binary()) -> true | false.
user_exists(Username) ->
    ecalendar_db_credential:user_exists(Username).
