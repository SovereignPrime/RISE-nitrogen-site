%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (tasks).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> "<i class='icon-calendar-empty icon-2x'></i>".

buttons() ->
    #panel{class='row-fluid', body=[

    #panel{class='span9 offset2', body=[
            #panel{class="row-fluid", body=[
                            #button{id=hide_show, class="btn btn-link span2", body="<i class='icon-angle-left'></i> Hide tasks", 
                                    actions=#event{type=click, actions=[
                                        #hide{trigger=hide_show,target=tasks}, 
                                        #event{postback=hide}
                                        ]}},
                            %#panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                            %#panel{ class='span2', body=},
                            common:render_filters(),
                            %#panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                            #link{id=archive, class='span2', body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}
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
    render_tasks(Parent, false).
render_tasks(Parent, Archive) ->
    CId = wf:session(current_task_id),
    case db:get_tasks(Parent, Archive) of
        {ok, Tasks} ->
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
                        #panel{class="btn btn-link", body = #link{body=[
                                    "<i class='icon-edit icon-large'></i><br>"      
                                    ], postback={edit, Id}, new=false}
                              },
                        #br{},
                        #panel{class="btn-group", body=[
                                #link{ class="btn btn-link droppdown-toggle", body=[
                                        "<i class='icon-reorder icon-large'></i>"
                                        ], new=false, data_fields=[{toggle, "dropdown"}]},
                                #list{numbered=false, class="dropdown-menu pull-right",
                                      body=[
                                        #listitem{body=[
                                                #link{body=[
                                                        "<i class='icon-list-alt icon-large'></i> Archive"
                                                        ], postback={archive, Task}, new=false}]}
                                        ]}

                                ]}
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

event({archive, #db_task{id=Id, parent=Parent} = Rec}) ->
    {ok, NTask} = db:archive(Rec),
    common:send_messages(NTask),
    wf:update(groups, render_tasks(Parent)),
    wf:update(subgroups, render_tasks(Id)),
    wf:update(body, render_task(Rec));
event({show_archive, true}) ->
    wf:update(groups, render_tasks(undefined, true)),
    wf:replace(archive, #link{id=archive, class='span2', body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    wf:update(subgroups, []);
event({show_archive, false}) ->
    wf:update(groups, render_tasks(undefined, false)),
    wf:replace(archive, #link{id=archive, class='span2', body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    wf:update(subgroups, []);
event({task_chosen, Id}) ->
    wf:session(current_task_id, Id),
    {ok, [ #db_task{parent=Parent, status=S} = Task ]} = db:get_task(Id),
    wf:session(current_task, Task),
    wf:update(groups, render_tasks(Parent, (S == archive))),
    wf:update(subgroups, render_tasks(Id,(S == archive))),
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
    wf:wire(body, [#remove_class{class="span8"}, #add_class{class="span12"}]),
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link span2", body="Show tasks <i class='icon-angle-right'></i>", 
                                    actions=#event{type=click, actions=[
                                        #show{trigger=hide_show,target=tasks}, 
                                        #event{postback=show}
                                        ]}});
event(show) ->
    wf:wire(body, [#remove_class{class="span12"}, #add_class{class="span8"}]),
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link span2", body="<i class='icon-angle-left'></i> Hide tasks", 
                                    actions=#event{type=click, actions=[
                                        #hide{trigger=hide_show,target=tasks}, 
                                        #event{postback=hide}
                                        ]}});
event(Click) ->
    io:format("~p~n",[Click]).

drop_event({task, Id}, subtask) ->
    PId = wf:session(current_task_id),
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
