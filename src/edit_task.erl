%% -*- mode: nitrogen -*-
-module (edit_task).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

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
                            "Subtask of: Example task 1", #br{},
                            "<i class='icon-th-large'></i> Edit/View task tree", #br{}
                            ]}
                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{ class="span12", body=[
                            "<i class='icon-file-alt'></i> Attachments", #br{},
                            #droppable{tag=filename, body=[
                                    #panel{ class="filedrop", body=[
                                            #br{}, "Drag and drop files here", #br{},#br{}
                                            ]}
                                    ]},

                            "<i class='icon-th-large'></i> Select from my files", #br{}
                            ]}
                    ]}
            ]}.

body() ->
    #panel{ class="span9", body=[
            #panel{ class="row-fluid", body=[
                    #panel{ class="input-prepend span12", body=[
                            #span{ class="add-on", body=[
                                    #span{ class="icon-stack",html_encode=false, text="<i class='icon-calendar-empty icon-stack-base'></i><i class='icon-small icon-ok'></i>"}
                                    ]},
                            #textbox{id=name, placeholder="Task name", text=wf:session_default(current_task, ""),  next=due, class="span11"}
                            ]}
                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{ class="input-prepend input-append span12", body=[
                            #span{ class="add-on", body=[
                                    #span{html_encode=false, text="<i class='icon-calendar'></i>"}
                                    ]},
                            #textbox{id=due, placeholder="Due", next=due, class="span9"},
                            #span{ class="add-on", body=[
                                    #span{ text="Calendar | Make recurring"}
                                    ]}
                            ]}
                    ]},
            #addable_row{id=roles, body= #involved{}},
            #panel{ class="row-fluid", body=[
                    #panel{class="span12", body=[
                            #textarea{class="input-block-level",rows=15, placeholder="Some text here", id=text}
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
    db:new_task(TaskName, Due, Text, []);

event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).
