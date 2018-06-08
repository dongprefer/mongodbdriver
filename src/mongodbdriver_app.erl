-module(mongodbdriver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:ensure_all_started(mongodb),
    application:ensure_all_started(mongrel),
    mongodbdriver_sup:start_link().

stop(_State) ->
    ok.
