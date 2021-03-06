-module(nitrogen_app).
-behaviour(application).
-export([start/2, stop/1]).
-include_lib("bitmessage/include/bm.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:load(mnesia),
    application:load(sasl),
    application:load(lager),
    application:load(etorrent_core),
    RiseDir = case os:type() of
        {win32, _} ->
            os:getenv("APPDATA") ++ "/RISE";
        {unix, linux} ->
                      os:getenv("HOME") ++ "/.config/RISE";
        _ ->
            os:getenv("HOME") ++ "/Library/RISE"
    end,
    file:make_dir(RiseDir),
    file:make_dir(RiseDir ++ "/data"),
    file:make_dir(RiseDir ++ "/scratch"),
    file:make_dir(RiseDir ++ "/log"),
    file:make_dir(RiseDir ++ "/log/sasl"),
    application:set_env(mnesia, dir, RiseDir ++ "/data"),
    application:set_env(etorrent_core, download_dir, RiseDir ++ "/scratch"),
    application:set_env(etorrent_core, dir, RiseDir ++ "/scratch"),
    application:set_env(etorrent_core, fast_resume_file, RiseDir ++ "/fast_resume_state.dets"),
    application:set_env(etorrent_core, logger_dir, RiseDir ++ "/log"),
    application:set_env(simple_bridge, scratch_dir, RiseDir ++ "/scratch"),
    application:set_env(sasl, sasl_error_logger, {file, RiseDir ++ "/log/sasl/sasl-error.log"}),
    application:set_env(lager, handlers,[
                                         {lager_file_backend,
                                          [{RiseDir ++ "/log/error.log", error, 10485760, "$D0", 5},
                                           {RiseDir ++ "/log/console.log", info, 10485760, "$D0", 5},
                                           {RiseDir ++ "/log/debug.log", debug, 10485760, "$D0", 5}
                                          ]}
                                        ]),
    application:set_env(lager, crash_log, RiseDir ++ "/log/crash.log"),
    application:start(mnesia),
    application:start(mimetypes),
    application:start(crypto),
    application:start(nprocreg),
    application:start(ranch),
    application:start(cowboy),
    mnesia:create_table(message, [{disc_copies, [node()]}, {attributes, record_info(fields, message)}, {type, set}, {record_name, message}]),
    application:start(bitmessage),
    db:update(4),
    etorrent:start_app(),
    % application:start(eminer),
    nitrogen_sup:start_link().

stop(_State) ->
    ok.
