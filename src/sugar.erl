-module(sugar).
-compile([export_all]).
-include_lib("bitmessage/include/bm.hrl").
-include("protokol.hrl").

date_format(Str) when is_list(Str)->  %{{{1
    Str;
date_format({{Y, M, D}, {H, Mi, S}}) ->  %{{{1
	%% might be worth replacing with qdate:to_string()
   	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Y, M, D, H, Mi, S]));
date_format({_Y, _M, _DD}=D) ->  %{{{1
    date_format({D, {9, 0, 0}}).

date_from_string("") ->  % {{{1
    "";
date_from_string(Str) when is_list(Str) ->  % {{{1
    [Y, M, D] = string:tokens(Str, "-"),
    {wf:to_integer(Y), wf:to_integer(M), wf:to_integer(D)};
date_from_string(Any) ->  % {{{1
    Any.


datetime_to_timestamp(DateTime) ->  %{{{1
    calendar:datetime_to_gregorian_seconds(DateTime) -
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

timestamp_to_datetime(TS) when is_integer(TS) -> % {{{1
    calendar:gregorian_seconds_to_datetime(TS +
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).

format_file_size(S) when S > 1000 * 1000 * 1000 ->  %{{{1
    wf:f("~.2fG", [ S  / bm_types:pow(1024, 3)]);
format_file_size(S) when S >  1000 * 1000 ->  %{{{1
    wf:f("~.2fM", [S / bm_types:pow(1024, 2)]);
format_file_size(S) when S > 1000 ->  %{{{1
    wf:f("~.2fK", [S / 1024]);
format_file_size(S) ->  %{{{1
    wf:f("~pb", [ S ]). 

format_timedelta(TD) when TD <  3600 ->  %{{{1
    wf:f("~p mins ago", [wf:to_integer(TD/60)]);
format_timedelta(TD) when TD < 24 * 3600 ->  %{{{1
    wf:f("~p hrs ago", [wf:to_integer(TD/3600)]);
format_timedelta(TD) ->  %{{{1
    wf:f("~p days ago", [wf:to_integer(TD/(24 * 3600))]).

sort_by_timestamp(Updates) ->  %{{{1
    lists:sort(fun(#message{text=A}, #message{text=B}) ->
                       F = fun(E) -> 
                                   try binary_to_term(E) of
                                           #message_packet{time=Z} when is_integer(Z) ->
                                               Z;
										   %% This accommodates protocol
										   %% oddness if packet is encoded with
										   %% older version of RISE
									       Task when element(1, Task) == task_packet ->
										       #task_packet{time=Z} = receiver:extract_task(Task),
                                               case is_integer(Z) of
												   true -> Z;
												   false -> bm_types:timestamp()
												end;
                                           _ ->
                                               bm_types:timestamp()
                                    catch 
                                       error:_P ->
                                           bm_types:timestamp()
                                   end
                           end,
                       F(A) > F( B )
               end, Updates).

maybe_wrap_list(AddressList) when is_list(AddressList) ->  % {{{1
    AddressList;
maybe_wrap_list(Address) when is_binary(Address) ->  % {{{1
    [Address].
