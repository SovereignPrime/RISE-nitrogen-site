-module(nitrogen_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(mnesia),
    application:start(bitmessage),
    case mnesia:wait_for_tables([db_group], 3000) of
        ok ->
            ok;
        {timeout, _} ->
            db:install()
    end,
    nitrogen_sup:start_link().

stop(_State) ->
    mnesia:stop(),
    ok.
