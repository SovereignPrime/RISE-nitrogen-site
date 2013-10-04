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
                            #panel{ class='span2', body="<i class='icon-arrow-left'></i> Back"},
                            #panel{ class='span2', body="<i class='icon-remove'></i> Discard"},
                            #panel{ class='span2', body="<i class='icon-ok'></i> Save"}
                            ]}
                    ]}
            ]}.

left() ->
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
            ]}.

body() ->
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
            ]}.
            
    
event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).

dropdown(Id, true=Icon) ->
    #dropdown{id=Id, html_encode=false, options=[
            #option{ text="<img src='img/globe.png'> Updates", value=updates},
            #option{ text="<img src='img/globe.png'> Tasks", value=tasks},
            #option{ text="<img src='img/globe.png'> Relationships", value=relations},
            #option{ text="<img src='img/globe.png'> Files", value=files},
            #option{ text="<img src='img/globe.png'> Finances", value=finance}
            ]};

dropdown(Id, false=Icon) ->
    #dropdown{id=Id, html_encode=false, options=[
            #option{ text="Updates", value=updates},
            #option{ text="Tasks", value=tasks},
            #option{ text="Relationships", value=relations},
            #option{ text="Files", value=files},
            #option{ text="Finances", value=finance}
            ]}.

