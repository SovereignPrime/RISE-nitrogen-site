%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (files).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Hello from relationships.erl!".

icon() -> "<i class='icon-file-text-alt icon-2x'></i>".

buttons() ->
    #panel{class='row-fluid', body=[

    #panel{class='span9 offset2', body=[
            #panel{class="row-fluid", body=[
                    #panel{ class='span2', body="<i class='icon-reorder'></i> More options"},
                    #panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                    #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                    #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                    #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                    ]}
            ]}]}.

left() ->
    [].

body() -> 
    [
        #table{ rows=[
                #tablerow{ cells=[
                        #tablecell{body=[
                                #checkbox{id=check_all,  postback=check_all, checked=false}
                                ], class=""},
                        #tableheader{text="File name", class=""},
                        #tableheader{text="Type", class=""},
                        #tableheader{text="Size", class=""},
                        #tableheader{text="From/To", class=""},
                        #tableheader{text="Linked message", class=""},
                        #tableheader{text="Date", class=""},
                        #tableheader{text="Seed", class=""},
                        #tableheader{text="Peer", class=""},
                        #tableheader{text="Status", class=""}
                        ]},
                #tablerow{ cells=[
                        #tablecell{body=[
                                #checkbox{id=check_all,  postback=check_all, checked=false}
                                ], class=""},
                        #tablecell{text="", class=""},
                        #tablecell{text="", class=""},
                        #tablecell{text="", class=""},
                        #tablecell{text="", class=""},
                        #tablecell{text="", class=""},
                        #tablecell{text="", class=""},
                        #tablecell{text="", class=""},
                        #tablecell{text="", class=""},
                        #tablecell{text="", class=""}
                        ]}
                ]}

        ].    
event(Click) ->
    io:format("~p~n",[Click]).
