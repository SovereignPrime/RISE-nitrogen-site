-module(nitrogen_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Timeout = appliation:get_env(nitrogen, db_timeout, 300),
    application:start(mnesia),
    application:start(mimetypes),
    etorrent:start_app(),
    application:start(bitmessage),
    case mnesia:wait_for_tables([db_group], Timeout) of
        ok ->
            ok;
        {timeout, _} ->
            db:install()
    end,
    nitrogen_sup:start_link().

stop(_State) ->
    %application:stop(cowboy),
    %application:stop(nprocreg),
    %application:stop(ranch),
    %application:stop(bitmessage),
    %application:stop(etorrent_core),
    %application:stop(crypto),
    %application:stop(mimetypes),
    %application:stop(mnesia),
    ok.
