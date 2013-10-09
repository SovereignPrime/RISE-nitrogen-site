%% -*- mode: nitrogen -*-
-module (edit_expense).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-usd icon-2x'></i>".

buttons() ->
    #panel{class='row-fluid', body=[
            #panel{class='span9 offset3', body=[
                    #panel{class="row-fluid", body=[
                            #panel{ class='span2', body="<i class='icon-arrow-left'></i> Back"},
                            #panel{ class='span2', body="<i class='icon-remove'></i> Discard"},
                            #button{ class='btn btn-link span2', body="<i class='icon-ok'></i> Save", postback=save, delegate=?MODULE}
                            ]}
                    ]}
            ]}.

left() ->
    #panel{ class="span3", body=[
            #panel{ class="row-fluid", body=[
                    #panel{ class="span6", body=[
                            #button{text="Log an expense", class="btn span12 active", postback=add_expense}
                            
                            ]},
                    #panel{ class="span6", body=[
                            #button{id=reorder, class="btn span12",  body="Log an assert", postback=reorder}
                            ]}

                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{ class="span12", body=[
                            #textbox{id="amount", text="Amount", next=order, class="input-block-level"}
                            ]}
                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{ class="span12", body=[
                            #panel{class="dropdown span12", body=[
                                    "<a href='#', class='span12 btn dropdown-toggle' data-toggle='dropdown'>",
                                    #span{ class="pull-left", text="Status: Responsible"},
                                    #span{ class="caret pull-right",html_encode=false, text=""},
                                    "</a>",
                                    #list{numbered=false, class="dropdown-menu",
                                          body=[
                                            #listitem{text="Responsible"},
                                            #listitem{text="Accountable"}
                                            ]}
                                    ]}
                            ]}
                    ]},
            #panel{ class="row-fluid", style="margin: 10% 0;", body=[
                    #panel{ class="span12", body=[
                            "<i class='icon-tasks'></i> For the tasks", #br{},
                            "Example task 1", #br{},
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
                                    "<i class='icon-usd'></i>"
                                    ]},
                            #textbox{id=name, text="Name", next=due, class="span11"}
                            ]}
                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{ class="input-prepend input-append span12", body=[
                            #span{ class="add-on", body=[
                                    #span{html_encode=false, text="<i class='icon-calendar'></i>"}
                                    ]},
                            #textbox{id=due, text="Due", class="span10"},
                            #span{ class="add-on", body=[
                                    #span{ text="Calendar | Make recurring"}
                                    ]}
                            ]}
                    ]},
            #addable_row{id=roles, body= #involved{}},
            #panel{ class="row-fluid", body=[
                    #panel{class="span12", body=[
                            #textarea{class="input-block-level",rows=15, text="Some text here", id=text}
                            ]}

                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{class="span12", body=[
                            #checkbox{id=notice,class="pull-left", text=" Send notice about this update to everyone involved",  checked=true}

                            ]}

                    ]}
            ]}.

event(save) ->
    Name = wf:q(name),
    Due = wf:q(due),
    Involved = wf:qs(person),
    Role = wf:qs(responsible),
    Payable = wf:qs(payable),
    Amount = wf:q(amount),
    Text = wf:q(text),
    db:new_expense(Name, Due, Text, Amount);

event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).
