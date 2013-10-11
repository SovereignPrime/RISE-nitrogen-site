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
                                        #event{postback=hide}
                                        ]}},
                            #panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                            #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                            #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                            #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                            ]}
                    ]}]}.

left() ->
    case 
        case wf:session(tid) of
            undefined  ->
                db:get_task();
            TId ->
                db:get_task(TId)
        end 
    of
        {ok, [ #db_task{id=CId, parent=Parent}=Task ]} ->
            wf:session(tid, CId),
            wf:session(current_task, Task),
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
                                                #droppable{tag=task, accept_groups=tasks, body=[
                                                        case db:get_tasks(Parent) of
                                                            {ok, Tasks} ->
                                                                #list{numbered=false,
                                                                      body=lists:map(fun(#db_task{name=Task, due=Due, id=Id}) when Id /= CId ->
                                                                                #task_leaf{tid=Id, name=Task, due=Due, delegate=?MODULE};
                                                                            (#db_task{name=Task, due=Due, id=Id}) when Id == CId ->
                                                                                #task_leaf{tid=Id, name=Task, due=Due, delegate=?MODULE, current=true}
                                                                        end, Tasks)
                                                                     };
                                                            {ok, [], undefined} ->
                                                                []
                                                        end
                                                        ]}
                                                ]},
                                        #panel{ class="span6", body=[
                                                #droppable{tag=subtask, accept_groups=tasks, body=[
                                                        case db:get_tasks(wf:session(tid)) of
                                                            {ok, []} ->
                                                                #list{numbered=false,
                                                                      body=
                                                                      ["&nbsp;",#br{},"&nbsp;",#br{},"&nbsp;",#br{},"&nbsp;",#br{},"&nbsp;",#br{}]};
                                                            {ok, Tasks} ->
                                                                #list{numbered=false,
                                                                      body=lists:map(fun(#db_task{name=Task, due=Due, id=Id}) ->
                                                                                #task_leaf{tid=Id, name=Task, due=Due, delegate=?MODULE}
                                                                        end, Tasks)
                                                                     };
                                                            {ok, [], undefined} ->
                                                                [#br{},#br{},#br{},#br{},#br{}]
                                                        end
                                                        ]}
                                                ]}
                                        ]}

                                ]}]}
                ];
        {ok, [], undefined} ->
            [];
        {ok, []} ->
            []
    end.

body() ->
    #db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}=Task = wf:session_default(current_task, #db_task{}),
    {ok, Involved} = db:get_involved(Id),
    %{ok, MyRole} = db:get_my_role(Id),
    #panel{id=body, class="span8", body=
           [
            #panel{ class="row-fluid", body=[
                    #panel{ class="span11", body=[
                            #h1{text=Name},
                            "Status: ", Status, #br{},
                            "Due: ", Due , #br{},
                            #br{},
                            "My role - ", "Accountable", #br{},
                            lists:map(fun({Name, Role}) ->
                                        [ Name, " - ", Role]
                                end, Involved)
                            ]},
                    #panel{ class="span1", body=[
                            #panel{class="", body = #link{body=[
                                        "<i class='icon-edit icon-large'></i><br>"      
                                        ], url="/edit_task", new=false}
                                  },
                            #panel{class="", body="<i class='icon-reorder icon-large'></i>"}
                            ]}


                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{ class="span12", body=Text}
                    ]},
            case db:get_attachments(Task) of
                {ok, []} ->
                    [];
                {ok, [], undefined} ->
                    [];
                {ok, Attachments} ->
                    #panel{class="row-fluid", body=[
                            #panel{class="span6", body="<i class='icon-file-alt'></i> Attachment"},
                            #panel{class="span2 offset4", body="<i class='icon-download-alt'></i> Download all"}
                            ]},
                    lists:map(fun(#db_file{path=Path, size=Size, date=Date, id=Id}) ->
                                #attachment{filename=Path, size=Size, time=Date, id=Id}
                        end, Attachments)
            end

            ]}.

event({task_chosen, Id}) ->
    wf:session(tid, Id),
    wf:wire(#event{actions=#script{script="window.location.reload(true);"}});
event(Click) ->
    io:format("~p~n",[Click]).

drop_event({task, Id}, subtask) ->
    PId = wf:session(tid),
    db:save_subtask(Id, PId);
drop_event({task, Id}, task) ->
    #db_task{parent=PId} = wf:session(current_task),
    db:save_subtask(Id, PId).
