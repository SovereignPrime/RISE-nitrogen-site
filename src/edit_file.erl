%% -*- mode: nitrogen -*-
-module (edit_file).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-file-text-alt icon-2x'></i>".

buttons() ->
    #panel{class='row-fluid', body=[
            #panel{class='span9 offset3', body=[
                    #panel{class="row-fluid", body=[
                            #panel{ class='span2', body="<i class='icon-arrow-left'></i> Back"},
                            #panel{ class='span2', body="<i class='icon-remove'></i> Discard"},
                            #panel{ class='span2', body="<i class='icon-ok'></i> Add to shared files"}
                            ]}
                    ]}
            ]}.

left() ->
    #panel{ class="span3", body=[
            #panel{ class="span12", body=[
                    "<i class='icon-plus'></i> Create new with uploaded files:"
                    ]},
            #panel{ class="span12", body=["Update"]},
            #panel{ class="span12", body=["Task"]},
            #panel{ class="span12", body=["Expense"]},
            #panel{ class="span12", body=["Assert"]}
            
            
            ]}.

body() ->
    #panel{ class="span9", body=[
            #panel{ class="row-fluid", body=[
                    #panel{ class="span12", body=[
                            "<i class='icon-file-alt'></i> Attachments", #br{},
                            #droppable{tag=filename, body=[
                                    #panel{ class="filedrop", body=[
                                            #br{}, "Drag and drop files here", #br{},#br{}
                                            ]}
                                    ]},

                            "<i class='icon-upload'></i> Classic upload", #br{}
                            ]}
                    ]}]}.
    
event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).

%dropdown(Id, true=Icon) ->
%    #dropdown{id=Id, html_encode=false, options=[
%            #option{ text="<img src='img/globe.png'> Updates", value=updates},
%            #option{ text="<img src='img/globe.png'> Tasks", value=tasks},
%            #option{ text="<img src='img/globe.png'> Relationships", value=relations},
%            #option{ text="<img src='img/globe.png'> Files", value=files},
%            #option{ text="<img src='img/globe.png'> Finances", value=finance}
%            ]};
%
%dropdown(Id, false=Icon) ->
%    #dropdown{id=Id, html_encode=false, options=[
%            #option{ text="Updates", value=updates},
%            #option{ text="Tasks", value=tasks},
%            #option{ text="Relationships", value=relations},
%            #option{ text="Files", value=files},
%            #option{ text="Finances", value=finance}
%            ]}.
%
