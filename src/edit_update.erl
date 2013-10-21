%% -*- mode: nitrogen -*-
-module (edit_update).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-globe icon-2x'></i>".

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
    Subject = wf:session(subject),
    {ok, Updates} = db:get_updates_by_subject(Subject),
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
                    case Updates of
                        [] ->
                            [];
                        U -> 
                            #panel{ class="span12", body=[
                                    "<i class='icon-file-alt'></i> Previous updates", #br{},
                                    [#update_preview{flag=false, age=Age, from=From, text=Text, icon="chevron-down"} || #db_update{from=From, date=Age, text=Text} <- U]
                                    ]}
                    end
                    ]}
            ]}.
body() ->
    #panel{ class="span9", body=[
            #panel{ class="row-fluid", body=[
                    #panel{ class="input-prepend span11", body=[
                            #span{ class="add-on", body=[
                                    #span{html_encode=false, text="<i class='icon-globe'></i>"}
                                    ]},
                            #textbox{id=name, placeholder="Re:something", next=due, class="span12"}
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
    Name = wf:q(name),
    Involved = wf:qs(person),
    Text = wf:q(text),
    db:new_update(Name, Text);

event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).
