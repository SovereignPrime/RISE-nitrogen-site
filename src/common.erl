-module(common).
-compile([export_all]).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> 
    case wf:user() of
        'undefined' ->
            wf:redirect_to_login("/login");
        R ->
            io:format("~p~n", [R]),
            {ok, Pid} = wf:comet_global(fun  incoming/0, incoming),
            receiver:register_receiver(Pid),
            T = #template { file="./site/templates/bare.html" },
            wf:wire('new_contact', #event{type=click, postback=add_contact, delegate=?MODULE}),
            wf:wire('new_group', #event{type=click, postback=add_group, delegate=?MODULE}),
            wf:wire('new_task', #event{type=click, postback=add_task, delegate=?MODULE}),
            wf:wire('new_expense', #event{type=click, postback=add_expense, delegate=?MODULE}),
            wf:wire('new_update', #event{type=click, postback=add_update, delegate=?MODULE}),
            T
    end.

render_files() ->
    {ok, Attachments} = db:get_files(wf:session_default(attached_files, [])),
    #panel{ class="span12", body=[
            "<i class='icon-file-alt'></i> Attachments", #br{},
            #upload{id=attachments, tag=filename, delegate=common, droppable=true,show_button=false, droppable_text="Drag and drop files here",  file_text=" Select my files"},
            lists:map(fun(#db_file{path=Path, size=Size, date=Date, id=Id}) ->
                        #attachment{filename=Path, size=Size, time=Date}
                end, Attachments)
            ]}.

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
    wf:session(attached_files, undefined),
    wf:redirect("/edit_task");
event(add_expense) ->
    {ok, Id} = db:next_id(db_expense),
    wf:session(current_expense_id, Id),
    wf:session(current_expense, #db_expense{id=Id}),
    wf:session(attached_files, undefined),
    wf:redirect("/edit_expense");
event(add_update) ->
    {ok, Id} = db:next_id(db_update),
    wf:session(current_subject, undefined),
    wf:session(current_update_id, Id),
    wf:session(current_update, #db_update{id=Id}),
    wf:session(attached_files, undefined),
    wf:redirect("/edit_update");
event(check_all) ->
    case wf:q(check_all) of
        "on" ->
            wf:replace(check, #checkbox{id=check,  postback=check_all, checked=true, delegate=common});
        undefined ->
            wf:replace(check, #checkbox{id=check,  postback=check_all, checked=false, delegate=common})
    end;
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

start_upload_event(_) ->
    ok.
finish_upload_event(filename, FName, FPath, _Node) ->
    FID = filename:basename(FPath),
    io:format("File uploaded: ~p to ~p for ~p~n", [FName, FPath, new]),
    [User] = wf:user(),
    db:save_file(FName, FPath,User),
    wf:session(attached_files, wf:session_default(attached_files, []) ++ [FID]),
    wf:update(files, render_files()).

incoming() ->
    receive
        update ->
            (wf:page_module()):incoming(),
            incoming()
    end.

save_involved(Type, TId) ->
    Involved = wf:qs(person),
    Role = wf:qs(responsible),
    io:format("~p ~p~n", [Involved, Role]),
    List = [ #db_contact_roles{type=Type, tid=TId, role=Role, contact=wf:session(wf:to_binary(Contact))} || {Contact, Role} <- lists:zip(Involved, Role), Involved /= [[]], Contact /= ""],
    lists:foreach(fun(P) -> 
                {ok, NPId} = db:next_id(db_contact_roles),
                db:save(P#db_contact_roles{id=NPId})
        end, List).

send_messages(#db_update{subject=Subject, text=Text, from=FID, to=Tos, date=Date}) ->
    ToIDs = [wf:session(wf:to_binary(T))|| T  <- Tos],
    #db_contact{address=From} = wf:user(),
    error_logger:info_msg("~p ~p~n", [ToIDs, From]),
    lists:foreach(fun(T) ->
                {ok, #db_contact{address=To}} = db:get_contact(T),
                bitmessage:send_message(From, wf:to_binary(To), wf:to_binary(Subject), wf:to_binary(Text), 2)
        end, ToIDs);
send_messages(#db_task{id=Id, name=Subject, text=Text, due=Date, parent=Parent, status=Status}) ->
    {ok, Involved} = db:get_involved(Id),
    % {_My, InvolvedN} =  lists:partition(fun({"Me", _, _}) -> true; (_) -> false end, Involved), 
    Contacts = [{C, R} || {_, R, C}  <- Involved],
    #db_contact{address=From} = wf:user(),
    %error_logger:info_msg("~p ~p~n", [Contacts, From]),
    InvolvedB =  <<"Involved:", << <<A/bytes, ":", (wf:to_binary(R))/bytes, ";">> || {#db_contact{bitmessage=A}, R} <- Contacts>>/bytes>>,
    lists:foreach(fun({#db_contact{address=To}, _}) ->
                bitmessage:send_message(From, wf:to_binary(To), wf:to_binary(Subject), wf:to_binary(<<Text/bytes, 10,  InvolvedB/bytes, 10, "Due:", (wf:to_binary(Date))/bytes, 10, "Status:", (wf:to_binary(Status))/bytes>>), 2)
        end, Contacts).


