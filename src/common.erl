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

event('add_task') ->
    wf:update(buttons, [
            #panel{class='span9 offset3', body=[
                    #panel{class="row-fluid", body=[
                            #panel{ class='span2', body="<i class='icon-arrow-left'></i> Back"},
                            #panel{ class='span2', body="<i class='icon-remove'></i> Discard"},
                            #panel{ class='span2', body="<i class='icon-ok'></i> Save"}
                            ]}
                    ]}
            ]),
    wf:update(main, [
            #panel{ class="span3", body=[
                    #h1{html_encode=false, text="<i class='icon-usd'></i> Payment"},
                    #panel{ class="row-fluid", body=[
                            #panel{ class="span8", body=[
                                    #textbox{id="person", text="John", next=amount, class="input-block-level"}
                                    ]},
                            #panel{ class="span3", body=[
                                    #textbox{id="amount", text="300$", next=order, class="input-block-level"}
                                    ]},
                            #panel{ class="span1", body=[
                                    #button{id=reorder, class="btn btn-block",  body="<i class='icon-reorder'></i>", postback=reorder}
                                    ]}
                            
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{ class="span8", body=[
                                    #textbox{id="person", text="Person", next=amount, class="input-block-level"}
                                    ]},
                            #panel{ class="span3", body=[
                                    #textbox{id="amount", text="Amount", next=order, class="input-block-level"}
                                    ]},
                            #panel{ class="span1", body=[
                                    #button{id=reorder, class="btn btn-block",  body="<i class='icon-plus'></i>", postback=reorder}
                                    ]}
                            
                            ]},
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
                    ]},
            #panel{ class="span9", body=[
                    #panel{ class="row-fluid", body=[
                            #panel{ class="input-prepend span12", body=[
                                    #span{ class="add-on span1", body=[
                                            #span{ class="icon-stack",html_encode=false, text="<i class='icon-calendar-empty icon-stack-base'></i><i class='icon-small icon-ok'></i>"}
                                            ]},
                                            #textbox{id=name, text="Task name", next=due, class="span11"}
                                    ]}
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{ class="input-prepend input-append span12", body=[
                                    #span{ class="add-on span1", body=[
                                            #span{html_encode=false, text="<i class='icon-calendar'></i>"}
                                            ]},
                                    #textbox{id=name, text="Due", next=due, class="span9"},
                                    #span{ class="add-on", body=[
                                            #span{ text="Calendar | Make recurring"}
                                            ]}
                                    ]}
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{ class="input-prepend span9", body=[
                                    #span{ class="add-on span1", body=[
                                            #span{html_encode=false, text="<i class='icon-user'></i>"}
                                            ]},
                                    #textbox{id=name, text="People", next=responsible, class="span11"}
                                    ]},
                            #panel{class="dropdown span2", body=[
                                    "<a href='#', class='btn dropdown-toggle' data-toggle='dropdown'>",
                                            #span{ class="", text="Is:Responsible"},
                                            #span{ class="caret",html_encode=false, text=""},
                                    "</a>",
                                    #list{numbered=false, class="dropdown-menu",
                                        body=[
                                            #listitem{text="Responsible"},
                                            #listitem{text="Accountable"}
                                            ]}
                                    ]},
                            #panel{class="span1", body=[
                                    #button{body="<i class='icon-plus'></i>", html_encode=false, postback=add_role}
                                    ]}
                            
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{class="span12", body=[
                                    #textarea{class="input-block-level",rows=15, text="Some text here", id=text}
                                    ]}
                            
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{class="span12", body=[
                                    #checkbox{id=notice,class="pull-left", text=" Send notice about this update to everyone involved",  checked=false}
                                    
                                    ]}
                            
                            ]}
                    ]}
            
            ]),
    io:format("Add task~n");
event('add_update') ->
    wf:update(buttons, [
            #panel{class='span9 offset3', body=[
                    #panel{class="row-fluid", body=[
                            #panel{ class='span2', body="<i class='icon-arrow-left'></i> Back"},
                            #panel{ class='span2', body="<i class='icon-remove'></i> Discard"},
                            #panel{ class='span2', body="<i class='icon-ok'></i> Save"}
                            ]}
                    ]}
            ]),
    wf:update(main, [
            #panel{ class="span3", body=[
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
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{ class="span12", body=[
                                    "<i class='icon-file-alt'></i> Previous updates", #br{},
                                    #update_preview{age="10 May (2 days ago)", from="John Smith", text="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean sit amet ligula non tellus scelerisque bibendum. Cras mollis elit eu nunc imperdiet, et dignissim velit cursus. Nulla sagittis velit in congue egestas. Vestibulum nunc diam, accumsan sit amet imperdiet sed, dictum vel est. Etiam consectetur, libero tempus ornare egestas, orci arcu placerat orci, nec tempor lectus tellus non dolor. Nulla sit amet est non enim mollis ultrices. Fusce et nulla sollicitudin, posuere nisl non, placerat metus. Donec fermentum turpis ut ligula feugiat mattis.", icon="chevron-down"}
                                    ]}
                            ]}
                    ]},
            #panel{ class="span9", body=[
                    #panel{ class="row-fluid", body=[
                            #panel{ class="input-prepend span12", body=[
                                    #span{ class="add-on span1", body=[
                                            #span{ class="icon-stack",html_encode=false, text="<i class='icon-calendar-empty icon-stack-base'></i><i class='icon-small icon-ok'></i>"}
                                            ]},
                                            #textbox{id=name, text="Task name", next=due, class="span11"}
                                    ]}
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{ class="input-prepend input-append span12", body=[
                                    #span{ class="add-on span1", body=[
                                            #span{html_encode=false, text="<i class='icon-calendar'></i>"}
                                            ]},
                                    #textbox{id=name, text="Due", next=due, class="span9"},
                                    #span{ class="add-on", body=[
                                            #span{ text="Calendar | Make recurring"}
                                            ]}
                                    ]}
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{ class="input-prepend span9", body=[
                                    #span{ class="add-on span1", body=[
                                            #span{html_encode=false, text="<i class='icon-user'></i>"}
                                            ]},
                                    #textbox{id=name, text="People", next=responsible, class="span11"}
                                    ]},
                            #panel{class="dropdown span2", body=[
                                    "<a href='#', class='btn dropdown-toggle' data-toggle='dropdown'>",
                                            #span{ class="", text="Is:Responsible"},
                                            #span{ class="caret",html_encode=false, text=""},
                                    "</a>",
                                    #list{numbered=false, class="dropdown-menu",
                                        body=[
                                            #listitem{text="Responsible"},
                                            #listitem{text="Accountable"}
                                            ]}
                                    ]},
                            #panel{class="span1", body=[
                                    #button{body="<i class='icon-plus'></i>", html_encode=false, postback=add_role}
                                    ]}
                            
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{class="span12", body=[
                                    #textarea{class="input-block-level",rows=15, text="Some text here", id=text}
                                    ]}
                            
                            ]},
                    #panel{ class="row-fluid", body=[
                            #panel{class="span12", body=[
                                    #checkbox{id=notice,class="pull-left", text=" Send notice about this update to everyone involved",  checked=false}
                                    
                                    ]}
                            
                            ]}
                    ]}
            
            ]),
    io:format("Add task~n");
event(E) ->
    io:format("Event ~p occured in ~p~n", [E, ?MODULE]).

dropevent(A, P) ->
    io:format("Drag ~p drop ~p~n", [A, P]).
