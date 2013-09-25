%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (relationships).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Hello from relationships.erl!".

icon() -> "user".

buttons() ->
    #panel{class='row-fluid', body=[

    #panel{class='span9 offset3', body=[
            #panel{class="row-fluid", body=[
                    #panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                    #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                    #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                    #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                    ]}
            ]}]}.

left() ->
    [
        #panel{class="span2", body=["test"]},
        #panel{class="span2", body=[
                "test"
                ]}

        ].
        

body() -> 
    #panel{class="span8", body=
    [
            #vcard{name="John Smith", address="132 Pavlin st.", email="test@test.org", phone="+7 231 123456787"}
            ]}.    
event(click) ->
    wf:insert_top(placeholder, "<p>You clicked the button!").
