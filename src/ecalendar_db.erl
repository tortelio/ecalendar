%%%-------------------------------------------------------------------
%%% @doc ecalendar public API for database management
%%%-------------------------------------------------------------------

-module(ecalendar_db).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start/0,
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

authenticate_user(Username, Password) ->
    case ecalendar_db_credential:find(Username) of
        [Username, Password] ->
            true;
        [Username | _] ->
            false;
        [] ->
            false
    end.

delete_event(Username, Eventname) ->
    ecalendar_db_calendar:delete_data(Username, Eventname).

insert_event_into_db(Key, Value) ->
    ecalendar_db_calendar:add_component(Key, Value).

event_exists(Event) ->
    ecalendar_db_calendar:is_exists(Event).

get_value(Key) ->
    ecalendar_db_calendar:get_component(Key).

get_user_list(Username) ->
    ecalendar_db_calendar:get_user_components(Username).

user_exists(Username) ->
    ecalendar_db_credential:user_exists(Username).
