-module(nitrogen_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:load(mnesia),
    case os:type() of
        {win32, _} ->
            application:set_env(mnesia, dir, os:getenv("APPDATA") ++ "/RISE/data");
        {unix, linux} ->
            application:set_env(mnesia, dir, os:getenv("HOME") ++ "/.config/RISE/data");
        _ ->
            application:set_env(mnesia, dir, os:getenv("HOME") ++ "/Library/RISE/data")
    end,
    application:start(mnesia),
    application:start(mimetypes),
    application:start(crypto),
    application:start(nprocreg),
    application:start(ranch),
    application:start(cowboy),
    application:start(bitmessage),
    etorrent:start_app(),
    application:start(eminer),
    nitrogen_sup:start_link().

stop(_State) ->
    case os:type() of
        {win32, _} ->
            TMP = os:getenv("TMP"),
            file:delete(TMP ++ "\\rise.port");
        _ ->
            file:delete("/tmp/rise.port")
    end,
    ok.
