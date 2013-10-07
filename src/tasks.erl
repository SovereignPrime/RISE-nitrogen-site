%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (tasks).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> #span{class="icon-small icon-stack", html_encode=false, text="<i class='icon-calendar-empty icon-stack-base'></i><i class='icon-ok'></i>"}.

buttons() ->
    #panel{class='row-fluid', body=[

    #panel{class='span9 offset2', body=[
            #panel{class="row-fluid", body=[
                    #panel{ class='span2', body="<i class='icon-angle-left'></i> Hide tasks"},
                    #panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                    #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                    #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                    #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                    ]}
            ]}]}.

left() ->
    {ok, {Tasks, _Cont}} = db:get_tasks(10),
    [
        #panel{class="span4", body=[
                #panel{ class="row-fluid", body=[
                        #list{numbered=false,class="pager",
                              body=[
                                #listitem{class="previous", body=[
                                        #link{html_encode=false, text="<i class='icon-arrow-left'></i>", postback={task_list, prrev}}
                                        ]},
                                #listitem{class="next",body=[
                                        #link{html_encode=false, text="<i class='icon-arrow-right'></i>", postback={task_list, prrev}}
                                        ]}
                                ]},
                        #panel{ class="row-fluid", body=[
                                #panel{ class="span6", body=[
                                        #list{numbered=false,
                                              body=lists:map(fun() ->
                                                #listitem{class="clearfix", body=[
                                                        #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
                                                        "<b>" ++ Task ++ "</b>", #br{},
                                                        "Due: " ++ Due
                                                        ]}, Tasks)
                                                ]}
                                ]},
                                #panel{ class="span6", body=[
                                        #list{numbered=false,
                                              body=[
                                                #listitem{class="clearfix", body=[
                                                        #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
                                                        "<b>Task 1</b>", #br{},
                                                        "Due: 25 Oct 2013"
                                                        ]},
                                                #listitem{class="clearfix", body=[
                                                        #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
                                                        "<b>Task 1</b>", #br{},
                                                        "Due: 25 Oct 2013"
                                                        ]},
                                                #listitem{class="clearfix", body=[
                                                        #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
                                                        "<b>Task 1</b>", #br{},
                                                        "Due: 25 Oct 2013"
                                                        ]},
                                                #listitem{class="clearfix", body=[
                                                        #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
                                                        "<b>Task 1</b>", #br{},
                                                        "Due: 25 Oct 2013"
                                                        ]},
                                                #listitem{class="clearfix", body=[
                                                        #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
                                                        "<b>Task 1</b>", #br{},
                                                        "Due: 25 Oct 2013"
                                                        ]},
                                                #listitem{class="clearfix", body=[
                                                        #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
                                                        "<b>Task 1</b>", #br{},
                                                        "Due: 25 Oct 2013"
                                                        ]},
                                                #listitem{class="clearfix", body=[
                                                        #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
                                                        "<b>Task 1</b>", #br{},
                                                        "Due: 25 Oct 2013"
                                                        ]},
                                                #listitem{class="clearfix", body=[
                                                        #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
                                                        "<b>Task 1</b>", #br{},
                                                        "Due: 25 Oct 2013"
                                                        ]}
                                                ]}
                                        ]}
                                ]}
                        
                        ]}]}
                ].
        

body() -> 
    #panel{class="span8", body=
    [
            #panel{ class="row-fluid", body=[
                    #panel{ class="span11", body=[
                            #h1{text="Example task 1"},
                            "Status: Created", #br{},
                            "Due: " ++ "31 Oct 2013", #br{},
                            #br{},
                            "My role - " ++ "Accountable", #br{},
                            "John Doe" ++ " - Responsible"
                            ]},
                    #panel{ class="span1", body=[
                            #panel{class="span1", body="<i class='icon-edit icon-large'></i><br><i class='icon-reorder icon-large'></i>"}
                            ]}
                    
                    
                    ]},
            "To create new contact user uses plus button, regardless of the screen he’s on. After clicking on ‘relationships’ he can either click on ‘new contact’ to bring this screen or drag and drop it on, for example, a group of contacts, or a task. First time user will have small info about drag’n’drop functionality which can be hidden.
            To create new contact user uses plus button, regardless of the screen he’s on. After clicking on ‘relationships’ he can either click on ‘new contact’ to bring this screen or drag and drop it on, for example, a group of contacts, or a task. First time user will have small info about drag’n’drop functionality which can be hidden.
            ",
            #panel{class="row-fluid", body=[
                    #panel{class="span6", body="<i class='icon-file-alt'></i> Attachment"},
                    #panel{class="span2 offset4", body="<i class='icon-download-alt'></i> Download all"}
                    ]},
            #attachment{filename="File1.xlsx", size="10mb", time="10/10/2013 10:59"},
            #attachment{filename="File1.xlsx", size="10mb", time="10/10/2013 10:59"},
            #attachment{filename="File1.xlsx", size="10mb", time="10/10/2013 10:59"}
            ]}.    
event(Click) ->
    io:format("~p~n",[Click]).
