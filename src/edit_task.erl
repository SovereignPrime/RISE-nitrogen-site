%% -*- mode: nitrogen -*-
-module (edit_task).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-task icon-2x'></i>".

buttons() ->
    #panel{class='row-fluid', body=[
            #panel{class='span9 offset3', body=[
                    #panel{class="row-fluid", body=[
                            #button{ class='btn btn-link span2', body="<i class='icon-remove'></i> Discard", 
   					click=#script{script="window.history.back();"}},
                            #button{ class='btn btn-link span2', body="<i class='icon-ok'></i> Save", postback=save, delegate=?MODULE}
                            ]}
                    ]}
            ]}.

left() ->
    #db_task{id=CId, parent=PId} = wf:session(current_task),
    PName = case db:get_task(PId) of
        {ok, [ #db_task{name=P} ]} ->
            P;
        _ ->
            undefined
    end,
    Children = case db:get_tasks(CId) of
        {ok, C} ->
            C;
        _ ->
            []
    end,
    #panel{ class="span3", body=[
            #h1{html_encode=false, text="<i class='icon-usd'></i> Payment"},
            #addable_row{id=payment, body=
                        #panel{class="row-fluid", body=[
                                #panel{ class="span8", body=[
                                        #textbox_autocomplete{id=payable,tag=contact, text="Name", next=amount, class="input-block-level", delegate=common}
                                        ]},
                                #panel{ class="span3", body=[
                                        #textbox{id=amount, placeholder="300$", next=order, class="input-block-level"}
                                        ]}
                                ]},
                         options=fun(Id, N) ->
                        []
                end},
            #panel{ class="row-fluid", style="margin: 10% 0;", body=[
                    #panel{ class="span12", body=[
                            "<i class='icon-tasks'></i> Linked tasks", #br{},
                            case PName of
                               undefined ->
                                   "";
                               N -> 
                                    [
                                        "Subtask of: ",N, #br{}
                                    ]
                            end,
                            lists:map(fun(#db_task{name=N}) ->
                                    [
                                        "Subtask: ",N, #br{}
                                    ]
                            end, Children),
                            #link{url="/tasks", body="<i class='icon-th-large'></i> Edit/View task tree"}, #br{}
                            ]}
                    ]},
            #panel{id=files, class="row-fluid", body=[
                    common:render_files()
                    ]}
            ]}.


body() ->
    #db_task{id=Id, name=Name, due=Due, text=Text} = wf:session(current_task),
    #panel{ class="span9", body=[
            #panel{ class="row-fluid", body=[
                    #panel{ class="input-prepend span12", body=[
                            #span{ class="add-on", body=[
                                    #span{ class="icon-stack",html_encode=false, text="<i class='icon-calendar-empty icon-stack-base'></i><i class='icon-small icon-ok'></i>"}
                                    ]},
                            #textbox{id=name, placeholder="Task name", text=Name,  next=due, class="span11"}
                            ]}
                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{ class="input-prepend input-append span12", body=[
                            #span{ class="add-on", body=[
                                    #span{html_encode=false, text="<i class='icon-calendar'></i>"}
                                    ]},
                            #datepicker_textbox{id=due,  next=due, text=Due, class="span9"},
                            #span{ class="add-on", body=[
                                    #span{ text="Calendar | Make recurring"}
                                    ]}
                            ]}
                    ]},
            #addable_row{id=roles, body= #involved{}},
            add_existing_rows(Id),
            #panel{ class="row-fluid", body=[
                    #panel{class="span12", body=[
                            #textarea{class="input-block-level",rows=15, placeholder="Some text here", id=text, text=Text}
                            ]}
                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{class="span12", body=[
                            #checkbox{id=notice,class="pull-left", text=" Send notice about this update to everyone involved",  checked=true}
                            ]}
                    ]}
            ]}.

add_existing_rows(Id) ->
    {ok, Involved} = db:get_involved(Id),
    Contacts = [{C, R} || {_, R, C}  <- Involved],
    Tos = lists:zip(Contacts, lists:seq(1, length(Contacts))),
    lists:foreach(fun({ {#db_contact{id=I, name=C}, R  }, N }) ->
                wf:session(wf:to_binary(C), I),
                element_addable_row:event({add, #addable_row{id=roles, num= N - 1, body=#involved{person=C, role=R}}})
        end, Tos),
    element_addable_row:event({del, #addable_row{id=roles, num= 0}}),
    element_addable_row:event({add, #addable_row{id=roles, num= length(Tos), body=#involved{}}}),
    [].
    
event(save) ->
    TaskName = wf:q(name),
    Due = wf:q(due),
    Text = wf:q(text),
    Id = wf:session(current_task_id),
    Task = wf:session(current_task),
    NTask = Task#db_task{name=wf:to_binary( TaskName ), due=Due, text=wf:to_binary(Text)},
    db:save(NTask),
    wf:session(current_task, NTask),
    db:save_attachments(wf:session(current_task), wf:session_default(attached_files, [])),
    save_payments(TaskName),
    common:save_involved(db_task, Id),
    common:send_messages(NTask),
    wf:session(task_attached_files, undefined),
    wf:redirect("/tasks");

event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).



%%%
%% Helpers
%%%
save_payments(TaskName) ->
    Payable = wf:qs(payable),
    Amounts = wf:qs(amount),
    #db_contact{id=UID} = wf:user(),
    Payments = [ #db_expense{name=TaskName, from=wf:session(wf:to_binary(Pay)), to=UID, amount=Am, status=new, type=expense} || {Pay, Am} <- lists:zip(Payable, Amounts)], 
    lists:foreach(fun(P) -> 
                {ok, NPId} = db:next_id(db_expense),
                db:save(P#db_expense{id=NPId}),
                db:save(#db_expense_tasks{task=wf:session(current_task_id), expense=NPId})
        end, Payments).

