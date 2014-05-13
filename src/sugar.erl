-module(sugar).
-compile([export_all]).
-include_lib("bitmessage/include/bm.hrl").
-include("protokol.hrl").

date_format(Str) when is_list(Str)->
    Str;
date_format({Y, M, D}) ->
	%% might be worth replacing with qdate:to_string()
   	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D])).

format_file_size(S) when S > 1000 * 1000 * 1000 ->
    wf:f("~.2fG", [ S  / bm_types:pow(1024, 3)]);
format_file_size(S) when S >  1000 * 1000 ->
    wf:f("~.2fM", [S / bm_types:pow(1024, 2)]);
format_file_size(S) when S > 1000 ->
    wf:f("~.2fK", [S / 1024]);
format_file_size(S) ->
    wf:f("~pb", [ S ]). 

format_timedelta(TD) when TD <  3600 ->
    wf:f("~p mins ago", [wf:to_integer(TD/60)]);
format_timedelta(TD) when TD < 24 * 3600 ->
    wf:f("~p hrs ago", [wf:to_integer(TD/3600)]);
format_timedelta(TD) ->
    wf:f("~p days ago", [wf:to_integer(TD/(24 * 3600))]).

sort_by_timestamp(Updates) ->
    lists:sort(fun(#message{text=A}, #message{text=B}) ->
                       F = fun(E) -> 
                                   try binary_to_term(E) of
                                           #message_packet{time=Z} ->
                                               Z;
                                           #task_packet{time=Z} ->
                                               Z
                                    catch 
                                       error:_P ->
                                           bm_types:timestamp()
                                   end
                           end,
                       F(A) > F( B )
               end, Updates).
