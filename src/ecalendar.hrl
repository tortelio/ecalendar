%%% ----------------------------------------------------------------------------
%%% @doc The header of ecalendar_app
%%% ----------------------------------------------------------------------------

-ifndef(ecalendar_hrl).
-define(ecalendar_hrl, true).
-define(APPLICATION, ecalendar).

%%%=============================================================================
%%% Types
%%%=============================================================================

%% representing a user of the server
-record cal_user, {name :: binary(),password :: binary()}.

-type cal_user() :: #cal_user{}.

-endif.
