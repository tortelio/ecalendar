%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @doc ecalendar public API
%%%-------------------------------------------------------------------

-module(ecalendar_app).

-behaviour(application).
%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start/2,
         stop/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the server. Call the appropriate handler.
start(_StartType, _StartArgs) ->
    % Start HTTP server
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/sign-up", sign_up_handler, []},
                                        {"/:username/calendar", calendar_handler, []},
                                        {"/:username/calendar/:component", component_handler, []}
                                       ]}]),

    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),

    % Start database
    ok = ecalendar_db:start(),
    ok = ecalendar_transform:start(),

    %% Load data
    %ecalendar_file:load_authorization_data(),
    %ecalendar_file:load_calendar_data(),
    ecalendar_sup:start_link().

%%--------------------------------------------------------------------
%% @doc Stop the server.
-spec stop(State :: any()) -> ok.
stop(_State) ->
    ok.
