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
    case wf:q(type) of 
        "new" ->
            wf:session(tid, undefined),
            wf:session(current_task, undefined);
        _ ->
            ok
    end,
    #db_task{id=CId, parent=PId} = wf:session_default(current_task, #db_task{}),
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
                                        #textbox{id=payable, placeholder="John", next=amount, class="input-block-level"}
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
                            #upload{tag=filename, delegate=?MODULE, droppable=true,show_button=false, droppable_text="Drag and drop files here",  file_text=" Select my files"}
                            ]}
                    ]}
            ]}.

body() ->
    #db_task{id=Id, name=Name, due=Due, text=Text} = wf:session_default(current_task, #db_task{}),
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
    case wf:session(tid) of
        'undefined' ->
            db:new_task(TaskName, Due, Text, undefined);
        Id ->
            #db_task{parent=Parent} = wf:session(current_task),
            db:save_task(Id, TaskName, Due, Text, Parent, "Changed")
        end;

event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).

start_upload_event(_) ->
    ok.
finish_upload_event(filename, FName, FPath, _Node) ->
    io:format("File uploaded: ~p to ~p~n", [FName, FPath]).
