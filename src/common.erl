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
            wf:wire('new_task', #event{type=click, postback=add_task, delegate=?MODULE}),
            wf:wire('new_expense', #event{type=click, postback=add_expense, delegate=?MODULE}),
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
event(add_task) ->
    {ok, Id} = db:next_id(db_task),
    wf:session(current_task_id, Id),
    wf:session(current_task, #db_task{id=Id}),
    wf:redirect("/edit_task");
event(add_expense) ->
    {ok, Id} = db:next_id(db_expense),
    wf:session(current_expense_id, Id),
    wf:session(current_expense, #db_expense{id=Id}),
    wf:redirect("/edit_expense");
event(E) ->
    io:format("Event ~p occured in ~p~n", [E, ?MODULE]).

dropevent(A, P) ->
    io:format("Drag ~p drop ~p~n", [A, P]).

autocomplete_enter_event(Term, _Tag) ->
    io:format("Term ~p~n", [Term]),
    {ok, Contacts} = db:all_contacts(),
    List = [{struct, [{id, Id}, {label, wf:to_binary(Name)}, {valie, Id}]} || #db_contact{id=Id, name=Name} <- Contacts, string:str(string:to_lower(wf:to_list(Name)), string:to_lower(Term)) > 0],
    mochijson2:encode(List).
autocomplete_select_event({struct, [{<<"id">>, K}, {<<"value">>, V}]} = Selected, _Tag) ->
    io:format("Selected ~p~n", [Selected]),
    wf:session(V, wf:to_integer(K)).

save_involved(Type, TId) ->
    Involved = wf:qs(person),
    Role = wf:qs(responsible),
    io:format("~p ~p~n", [Involved, Role]),
    List = [ #db_contact_roles{type=Type, tid=TId, role=Role, contact=wf:session(wf:to_binary(Contact))} || {Contact, Role} <- lists:zip(Involved, Role), Involved /= [[]], Contact /= ""],
    lists:foreach(fun(P) -> 
                {ok, NPId} = db:next_id(db_contact_roles),
                db:save(P#db_contact_roles{id=NPId})
        end, List).
