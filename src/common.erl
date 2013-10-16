-module(common).
-compile([export_all]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> 
    case wf:user() of
        'undefined' ->
            wf:redirect_to_login("/login");
        R ->
            io:format("~p~n", [R]),
            T = #template { file="./site/templates/bare.html" },
            wf:wire('new_contact', #event{type=click, postback=add_contact, delegate=?MODULE}),
            wf:wire('new_group', #event{type=click, postback=add_group, delegate=?MODULE}),
            T
    end.

event(add_group) ->
    {ok, Id} = db:next_id(db_group),
    db:save(#db_group{
            id=Id,
            name="New group",
            subgroups=undefined
            }),
    wf:redirect("/relationships");
event(add_contact) ->
    {ok, Id} = db:next_id(db_contact),
    wf:session(current_contact, undefined),
    wf:session(current_contact_id, Id),
    db:save(#db_contact{
            id=Id,
            name="Contact Name"
            }),
    wf:redirect("/relationships");
event(E) ->
    io:format("Event ~p occured in ~p~n", [E, ?MODULE]).

dropevent(A, P) ->
    io:format("Drag ~p drop ~p~n", [A, P]).
