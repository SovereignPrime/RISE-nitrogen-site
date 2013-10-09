%% -*- mode: nitrogen -*-
-module (edit_update).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-globe icon-2x'></i>".

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
                            #update_preview{flag=false, age="10 May (2 days ago)", from="John Smith", text="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean sit amet ligula non tellus scelerisque bibendum. Cras mollis elit eu nunc imperdiet, et dignissim velit cursus. Nulla sagittis velit in congue egestas. Vestibulum nunc diam, accumsan sit amet imperdiet sed, dictum vel est. Etiam consectetur, libero tempus ornare egestas, orci arcu placerat orci, nec tempor lectus tellus non dolor. Nulla sit amet est non enim mollis ultrices. Fusce et nulla sollicitudin, posuere nisl non, placerat metus. Donec fermentum turpis ut ligula feugiat mattis.", icon="chevron-down"}
                            ]}
                    ]}
            ]}.
body() ->
    #panel{ class="span9", body=[
            #panel{ class="row-fluid", body=[
                    #panel{ class="input-prepend span11", body=[
                            #span{ class="add-on", body=[
                                    #span{html_encode=false, text="<i class='icon-globe'></i>"}
                                    ]},
                            #textbox{id=name, text="Re:something", next=due, class="span12"}
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
                            #checkbox{id=notice,class="pull-left", text=" Send notice about this update to everyone involved",  checked=false}

                            ]}

                    ]}
            ]}.
            
    
event(save) ->
    Name = wf:q(name),
    Involved = wf:qs(person),
    Text = wf:q(text),
    db:new_update(Name, Text);

event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).
