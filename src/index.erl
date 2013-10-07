%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-globe icon-2x'></i>".

buttons() ->
    #panel{class='row-fluid', body=[
            #panel{class='span1 offset1', body=[
                    #span{ class='label label-inverse',text="10 new"}
                    ]},
            #panel{class='span9 offset1', body=[
                    #panel{class="row-fluid", body=[
                            #panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                            #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                            #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                            #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                            ]}
                    ]}]}.

left() ->
    #panel{class="span3", body=
    [ E || _ <- lists:seq(1,3), E <- [
    #update_preview{icon="globe", from="John Smith", age="3 days", subject="Lorem ipisiun", text="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut convallis egestas neque, sit amet mollis nisi tincidunt in. Proin fringilla sem vitae enim egestas, ut rutrum diam hendrerit. Nulla facilisi. Curabitur eleifend libero quam, sit amet sodales odio porttitor eget. Integer sit amet consequat magna. Ut eget tempus augue. Donec sodales suscipit ipsum, sed interdum nisl tincidunt a. In pretium mi ac viverra auctor. Nam dapibus interdum lectus et posuere."}
                    ]]}.

body() ->
    #panel{class="span9", body=
    [
        #h1{html_encode=false, text="<i class='icon-globe'></i> Subject goes here"},
        #update_element{collapse=true, from="John Smith", text="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean sit amet ligula non tellus scelerisque bibendum. Cras mollis elit eu nunc imperdiet, et dignissim velit cursus. Nulla sagittis velit in congue egestas. Vestibulum nunc diam, accumsan sit amet imperdiet sed, dictum vel est. Etiam consectetur, libero tempus ornare egestas, orci arcu placerat orci, nec tempor lectus tellus non dolor. Nulla sit amet est non enim mollis ultrices. Fusce et nulla sollicitudin, posuere nisl non, placerat metus. Donec fermentum turpis ut ligula feugiat mattis.", age="10 Sep (10 days ago)"},
        #update_element{collapse=true, from="John Smith", text="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean sit amet ligula non tellus scelerisque bibendum. Cras mollis elit eu nunc imperdiet, et dignissim velit cursus. Nulla sagittis velit in congue egestas. Vestibulum nunc diam, accumsan sit amet imperdiet sed, dictum vel est. Etiam consectetur, libero tempus ornare egestas, orci arcu placerat orci, nec tempor lectus tellus non dolor. Nulla sit amet est non enim mollis ultrices. Fusce et nulla sollicitudin, posuere nisl non, placerat metus. Donec fermentum turpis ut ligula feugiat mattis.", age="10 Sep (10 days ago)"},
        #update_element{collapse=false, from="John Smith", text="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean sit amet ligula non tellus scelerisque bibendum. Cras mollis elit eu nunc imperdiet, et dignissim velit cursus. Nulla sagittis velit in congue egestas. Vestibulum nunc diam, accumsan sit amet imperdiet sed, dictum vel est. Etiam consectetur, libero tempus ornare egestas, orci arcu placerat orci, nec tempor lectus tellus non dolor. Nulla sit amet est non enim mollis ultrices. Fusce et nulla sollicitudin, posuere nisl non, placerat metus. Donec fermentum turpis ut ligula feugiat mattis.", age="10 Sep (10 days ago)"},
        #panel{class="row-fluid", body=[
                #panel{class="span3 offset4", body=[
                        #span{class="icon-reply icon-large", text=" "},
                        #span{class="icon-refresh icon-large", text=" "},
                        #span{class="icon-reorder icon-large"}
                        ]}]},
        #panel{class="row-fluid", body=[
                #panel{class="span6", body="<i class='icon-file-alt'></i> Attachment"},
                #panel{class="span2 offset4", body="<i class='icon-download-alt'></i> Download all"}
                ]},
        #attachment{filename="File1.xlsx", size="10mb", time="10/10/2013 10:59"},
        #attachment{filename="File1.xlsx", size="10mb", time="10/10/2013 10:59"},
        #attachment{filename="File1.xlsx", size="10mb", time="10/10/2013 10:59"}


            ]}.
	
    
event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).

