-module(sugar).
-compile([export_all]).

date_format(Str) when is_list(Str)->
    Str;
date_format({Y, M, D}) ->
    io_lib:format("~p-~p-~p", [Y, M, D]).

format_file_size(S) when S > 1000 * 1000 * 1000 ->
    wf:f("~.2fG", [ S  / bm_types:pow(1024, 3)]);
format_file_size(S) when S >  1000 * 1000 ->
    wf:f("~.2fM", [S / bm_types:pow(1024, 2)]);
format_file_size(S) when S > 1000 ->
    wf:f("~.2fK", [S / 1024]);
format_file_size(S) ->
    S. 

