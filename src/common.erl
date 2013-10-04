-module(common).
-compile([export_all]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> 
    Temp = #template { file="./site/templates/bare.html" },
    wf:wire('new_task', 'plus_menu', #event{type=click, postback=add_task, delegate=?MODULE}),
    wf:wire('new_update', 'plus_menu', #event{type=click, postback=add_update, delegate=?MODULE}),
    wf:wire('new_relationship', 'plus_menu', #event{type=click, postback=add_relation, delegate=?MODULE}),
    wf:wire('new_file', 'plus_menu', #event{type=click, postback=add_file, delegate=?MODULE}),
    wf:wire('new_expence', 'plus_menu', #event{type=click, postback=add_expence, delegate=?MODULE}),
    Temp.

event(E) ->
    io:format("Event ~p occured in ~p~n", [E, ?MODULE]).

dropevent(A, P) ->
    io:format("Drag ~p drop ~p~n", [A, P]).
