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
                                        #textbox_autocomplete{id=payable,tag=payable, text="Name", next=amount, class="input-block-level"}
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
            #panel{ class="row-fluid", body=[
                    #panel{ class="span12", body=[
                            "<i class='icon-file-alt'></i> Attachments", #br{},
                            #upload{id=attachments, tag=filename, delegate=?MODULE, droppable=true,show_button=false, droppable_text="Drag and drop files here",  file_text=" Select my files"}
                            ]}
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
            
    
event(save) ->
    TaskName = wf:q(name),
    Due = wf:q(due),
    Involved = wf:qs(person),
    Role = wf:qs(responsible),
    Payable = wf:qs(payable),
    Amounts = wf:qs(amount),
    Text = wf:q(text),
    Id = wf:session(current_task_id),
    Task = wf:session(current_task),
    NTask = Task#db_task{name=TaskName, due=Due, text=Text},
    io:format("Payable ~p sum ~p~n", [Payable, Amounts]),
    db:save(NTask),
    wf:session(current_task, NTask),
    db:save_attachments(wf:session(current_task), wf:session_default(task_attached_files, [])),
    wf:redirect("/tasks");

event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).

start_upload_event(_) ->
    ok.
finish_upload_event(filename, FName, FPath, _Node) ->
    FID = filename:basename(FPath),
    io:format("File uploaded: ~p to ~p for ~p~n", [FName, FPath, new]),
    db:save_file(FName, FPath, wf:user()),
    wf:session(task_attached_files, wf:session_default(task_attached_files, []) ++ [FID]).

autocomplete_enter_event(Term, _Tag) ->
    io:format("Term ~p~n", [Term]),
    {ok, Contacts} = db:all_contacts(),
    List = [{struct, [{id, Id}, {label, wf:to_binary(Name)}, {valie, Id}]} || #db_contact{id=Id, name=Name} <- Contacts, string:str(string:to_lower(wf:to_list(Name)), string:to_lower(Term)) > 0],
    mochijson2:encode(List).
autocomplete_select_event(Selected, _Tag) ->
    io:format("Selected ~p~n", [Selected]).
