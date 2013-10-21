-module(sugar).
-compile([export_all]).

date_format({Y, M, D}) ->
    io_lib:format("~p-~p-~p", [Y, M, D]).
