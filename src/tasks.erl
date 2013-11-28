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
    CId = wf:session(current_task_id),
    [
        #panel{id=tasks, class="span4", body=[
                #panel{ class="row-fluid", body=[
                        #list{numbered=false,class="pager",
                              body=[
                                #listitem{class="previous", body=[
                                        #link{html_encode=false, text="<i class='icon-arrow-left'></i>", postback={task_list, prrev}}
                                        ]}
                                ]},
                        #panel{ class="row-fluid", body=[
                                #panel{class="span6", body=[
                                        #droppable{id=groups, tag=task, accept_groups=tasks, body=[
                                                render_tasks(undefined)
                                                ]}
                                        ]},
                                #panel{class="span6", body=[
                                        #droppable{id=subgroups, style="height:100%;", tag=subtask, accept_groups=tasks, body=[
                                                if 
                                                    CId /= undefined ->
                                                        render_tasks(CId);
                                                    true ->
                                                        []
                                                end
                                                ]}
                                        ]}
                                ]}

                        ]}]}
        ].

render_tasks(Parent) ->
    CId = wf:session(current_task_id),
    io:format("~p ~p~n",[Parent,CId]),
    case db:get_tasks(Parent) of
        {ok, Tasks} ->
            io:format("~p~n", [Tasks]),
            [
                #list{numbered=false,
                      body=lists:map(fun(#db_task{name=Task, due=Due, id=Id}) when Id /= CId ->
                                #task_leaf{tid=Id, name=Task, due=Due, delegate=?MODULE};
                            (#db_task{name=Task, due=Due, id=Id}) when Id == CId ->
                                #task_leaf{tid=Id, name=Task, due=Due, delegate=?MODULE, current=true}
                        end, Tasks)
                     },
                "&nbsp;", #br{},#br{},#br{}
                ];
        {ok, [], undefined} ->
            [
                "&nbsp;", #br{},#br{},#br{}
                ]
    end.

body() ->
    #db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}=Task = wf:session_default(current_task, #db_task{}),
    #panel{id=body, class="span8", body=
           [
            render_task(Task)
            ]}.
render_task(#db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}=Task) ->
    {ok, Involved} = db:get_involved(Id),
    {My, InvolvedN} = case lists:partition(fun({"Me", _, _}) -> true; (_) -> false end, Involved) of
        {[{_,M, _}], I} ->
            {M, I};
        {[], I} ->
            {no, I}
    end,

    [
        #panel{ class="row-fluid", body=[
                #panel{ class="span11", body=[
                        #h1{text=Name},
                        "Status: ", Status, #br{},
                        "Due: ", Due , #br{},
                        #br{},
                        "My role - ", My, #br{},
                        lists:map(fun({Name, Role, _}) ->
                                    [ Name, " - ", Role, #br{}]
                            end, InvolvedN)
                        ]},
                #panel{ class="span1", body=[
                        #panel{class="", body = #link{body=[
                                    "<i class='icon-edit icon-large'></i><br>"      
                                    ], postback={edit, Id}, new=false}
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
                [
                    #panel{class="row-fluid", body=[
                            #panel{class="span6", body="<i class='icon-file-alt'></i> Attachment"},
                            #panel{class="span2 offset4", body="<i class='icon-download-alt'></i> Download all"}
                            ]},
                    lists:map(fun(#db_file{path=Path, size=Size, date=Date, id=Id, status=State}) ->
                                #attachment{fid=Id, filename=Path, size=Size, time=Date, status=State}
                        end, Attachments)
                    ]
        end
        ].

event({task_chosen, Id}) ->
    wf:session(current_task_id, Id),
    {ok, [ #db_task{parent=Parent} = Task ]} = db:get_task(Id),
    wf:session(current_task, Task),
    wf:update(groups, render_tasks(Parent)),
    wf:update(subgroups, render_tasks(Id)),
    wf:update(body, render_task(Task));
event({task_list, prrev}) ->
    #db_task{id=Id, parent=Parent} = wf:session(current_task),
    wf:session(current_task_id,Parent),
    {ok, [#db_task{parent=PP}=PT]} = db:get_task(Parent),
    wf:session(current_task, PT),
    wf:update(subgroups, render_tasks(Parent)),
    wf:update(groups, render_tasks(PP)),
    wf:update(body, render_task(PT));
event({edit, Id}) ->
    Task = wf:session(current_task),
    wf:session(current_task, Task#db_task{status=changed}),
    wf:redirect("/edit_task");
event(hide) ->
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link span2", body="Show tasks <i class='icon-angle-right'></i>", 
                                    actions=#event{type=click, actions=[
                                        #show{trigger=hide_show,target=tasks}, 
                                        #event{postback=show}
                                        ]}});
event(show) ->
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link span2", body="<i class='icon-angle-left'></i> Hide tasks", 
                                    actions=#event{type=click, actions=[
                                        #hide{trigger=hide_show,target=tasks}, 
                                        #event{postback=hide}
                                        ]}});
event(Click) ->
    io:format("~p~n",[Click]).

drop_event({task, Id}, subtask) ->
    PId = wf:session(current_task_id),
    io:format("Task ~p to ~p~n", [Id,PId]),
    if PId /= Id ->
            db:save_subtask(Id, PId);
        true ->
            ok
    end;
drop_event({task, Id}, task) ->
    #db_task{parent=PId} = wf:session(current_task),
    db:save_subtask(Id, PId).
incoming() ->
    CT = wf:session(current_task_id),
    {ok, [#db_task{parent=PP}]} = db:get_task(CT),
    wf:update(groups, tasks:render_tasks(PP)),
    wf:update(subgroups, tasks:render_tasks(CT)),
    wf:flush().
