-module(nitrogen_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(mnesia),
    application:start(mimetypes),
    application:start(crypto),
    application:start(nprocreg),
    application:start(ranch),
    application:start(cowboy),
    etorrent:start_app(),
    application:start(eminer),
    pat:start(),
    nitrogen_sup:start_link().

stop(_State) ->
    ok.
