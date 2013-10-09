%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (tasks).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> #span{class="icon-small icon-stack", html_encode=false, text="<i class='icon-calendar-empty icon-stack-base'></i><i class='icon-ok'></i>"}.

buttons() ->
    #panel{class='row-fluid', body=[

    #panel{class='span9 offset2', body=[
            #panel{class="row-fluid", body=[
                            #button{id=hide_show, class="btn btn-link span2", body="<i class='icon-angle-left'></i> Hide tasks", 
                                    actions=#event{type=click, actions=[
                                        #hide{trigger=hide_show,target=tasks}, 
                                        #remove_class{target=body, class=span8},
                                        #add_class{target=body, class=span12},
                                        #event{postback=hide}
                                        ]}},
                            #panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                            #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                            #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                            #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                            ]}
                    ]}]}.

left() ->
    {ok, {Tasks, Cont}} = db:get_tasks(10),
    %{ok, {Tasks1, _Cont}} = db:get_tasks(Cont,10),
    [
        #panel{id=tasks, class="span4", body=[
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
                                              body=lists:map(fun(#db_task{name=Task, due=Due, id=Id}) ->
                                                        #listitem{class="clearfix", body=[
                                                                #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback={check, Id}, checked=false},
                                                                "<b>" ++ Task ++ "</b>", #br{},
                                                                "Due: " ++ Due
                                                                ]}
                                                end, Tasks)
                                                }
                                ]}%,
%                                #panel{ class="span6", body=[
%                                        #list{numbered=false,
%                                              body=lists:map(fun(#db_task{name=Task, due=Due}) ->
%                                                        #listitem{class="clearfix", body=[
%                                                                #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback=test, checked=false},
%                                                                "<b>" ++ Task ++ "</b>", #br{},
%                                                                "Due: " ++ Due
%                                                                ]}
%                                                end, Tasks1)
                                             %}
                                        %]}
                                ]}
                        
                        ]}]}
                ].
        

body() -> 
    {ok, #db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}} = case wf:session(tid) of
        undefined  ->
            db:get_task();
        TId ->
            db:get_task(TId)
    end,

    #panel{id=body, class="span8", body=
    [
            #panel{ class="row-fluid", body=[
                    #panel{ class="span11", body=[
                            #h1{text=Name},
                            "Status: ", Status, #br{},
                            "Due: ", Due , #br{},
                            #br{},
                            "My role - ", "Accountable", #br{},
                            "John Doe", " - Responsible"
                            ]},
                    #panel{ class="span1", body=[
                            #panel{class="span1", body="<i class='icon-edit icon-large'></i><br><i class='icon-reorder icon-large'></i>"}
                            ]}
                    
                    
                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{ class="span12", body=Text}
                    ]},
            #panel{class="row-fluid", body=[
                    #panel{class="span6", body="<i class='icon-file-alt'></i> Attachment"},
                    #panel{class="span2 offset4", body="<i class='icon-download-alt'></i> Download all"}
                    ]},
            #attachment{filename="File1.xlsx", size="10mb", time="10/10/2013 10:59"},
            #attachment{filename="File1.xlsx", size="10mb", time="10/10/2013 10:59"},
            #attachment{filename="File1.xlsx", size="10mb", time="10/10/2013 10:59"}
            ]}.    

%event(hide_tasks) ->
%    wf:
event(Click) ->
    io:format("~p~n",[Click]).
