%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (tasks).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> #image{image="/img/tasks.svg", class="icon", style="height: 32px;"}.


buttons(main) ->  % {{{1
    #list{numbered=false, class="nav nav-pills", style="display:inline-block",
          body=[
%                #listitem{body=[
%
%                                %#panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
%                               ]},
                #listitem{body=[
                                #button{id=hide_show, class="btn btn-link", body="<i class='icon-angle-left'></i> Hide tasks", 
                                        actions=#event{type=click, actions=[
                                                                            #hide{trigger=hide_show,target=tasks}, 
                                                                            #event{postback=hide}
                                                                           ]}}
                               ]},
                #listitem{body=[
                                common:render_filters()
                               ]},
%                #listitem{body=[
%                                %#panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
%                               ]},
                #listitem{body=[
                                #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}
                               ]},
                #listitem{body=[
                                common:settings_menu()
                               ]}
                    ]}.

left() ->  % {{{1
    CId = wf:session(current_task_id),
    Par = case wf:session(current_task) of  % {{{2
              #db_task{id=CId, parent=Parent} ->
                  %{ok, [#db_task{parent=PParent} ]} = db:get_task(Parent),
                  wf:session(left_parent_id, Parent),
                  wf:session(right_parent_id, CId),
                  Parent;
              _->
                  undefined
          end,
    [
        #panel{id=tasks, class="span4", body=[
                #panel{ class="row-fluid", body=[
                        #list{numbered=false,class="pager",
                              body=[
                                #listitem{class="previous", body=[
                                        #link{html_encode=false, text="<i class='icon-arrow-left'></i>", postback={task_list, prrev}}
                                        ]},
                                #listitem{class="next", body=[
                                        #link{html_encode=false, text="<i class='icon-arrow-right'></i>", postback={task_list, next}}
                                        ]}
                                ]},
                        #panel{ class="row-fluid", body=[
                                #panel{class="span6", body=[
                                        #droppable{id=groups, tag=task, accept_groups=tasks, body=[
                                                render_tasks(Par)
                                                ]}
                                        ]},
                                #panel{class="span6", body=[
                                        #droppable{id=subgroups, style="height:100%;", tag=subtask, accept_groups=tasks, body=[
                                                if CId /= Par, CId /= undefined ->  % {{{3
                                                        render_tasks(CId);
                                                    true ->
                                                        []
                                                end
                                                ]}
                                        ]}
                                ]}

                        ]}]}
        ].


render_tasks() ->  % {{{1
    Left = wf:session(left_parent_id),
    Right = wf:session(right_parent_id),
    wf:update(groups, render_tasks(Left)),
    wf:update(subgroups, render_tasks(Right)).
render_tasks(Parent) ->  % {{{1
    render_tasks(Parent, false).
render_tasks(Parent, Archive) ->  % {{{1
    CId = wf:session(current_task_id),
    Par = wf:session(right_parent_id),
    case db:get_tasks(Parent, Archive) of  % {{{2
        {ok, Tasks} ->
            [
                #list{numbered=false,
                      body=lists:map(fun(#db_task{name=Task, due=Due, id=Id}) when Id == CId; Id == Par ->  % {{{3
                                             #task_leaf{tid=Id, name=Task, due=Due, delegate=?MODULE, current=true};
                                        (#db_task{name=Task, due=Due, id=Id})  ->  % {{{3
                                             #task_leaf{tid=Id, name=Task, due=Due, delegate=?MODULE}
                                     end, Tasks)
                     },
                "&nbsp;", #br{},#br{},#br{}
                ];
        {ok, [], undefined} ->
            [
                "&nbsp;", #br{},#br{},#br{}
                ]
    end.

body() ->  % {{{1
    #db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}=Task = wf:session_default(current_task, #db_task{text=""}),
    #panel{id=body, class="span8", body=
           [
            render_task(Task)
            ]}.
render_task(#db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}=Task) ->  % {{{1
    {ok, Involved} = db:get_involved(Id),
    {My, InvolvedN} = case lists:partition(fun({"Me", _, _}) -> true; (_) -> false end, Involved) of
        {[{_,M, _}], I} ->  % {{{1
            {M, I};
        {[], I} ->  % {{{1
            {no, I}
    end,
    TextF = re:replace(Text, "\r*\n", "<br>", [{return, list}, noteol, global]), 
    {ok, Updates} = db:get_task_history(Id),
    io:format("Upd: ~p~n", [Updates]),

    [
        #panel{ class="row-fluid", body=[
                #panel{ class="span11", body=[
                        #h1{text=Name},
                        "Status: ", Status, #br{},
                        "Due: ", Due , #br{},
                        #br{},
                        "My role - ", My, #br{},
                        lists:map(fun({Name, Role, _}) ->  % {{{1
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
                #panel{ class="span12", body=TextF}
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
        end,
        [
         #update_element{collapse=true, from=From, to=To, text=Text, uid=Id, subject=Subject, enc=Enc, status=Status} || #message{hash=Id, enc=Enc, to=To, subject=Subject, from=From, text=Text, status=Status} <- sugar:sort_by_timestamp(Updates)
        ] 
    ].

event({archive, #db_task{id=Id, parent=Parent} = Rec}) ->  % {{{1
    {ok, NTask} = db:archive(Rec),
    common:send_messages(NTask),
    wf:update(groups, render_tasks(Parent)),
    wf:update(subgroups, render_tasks(Id)),
    wf:update(body, render_task(Rec));
event({show_archive, true}) ->  % {{{1
    wf:update(groups, render_tasks(undefined, true)),
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    wf:update(subgroups, []);
event({show_archive, false}) ->  % {{{1
    wf:update(groups, render_tasks(undefined, false)),
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    wf:update(subgroups, []);
event({task_chosen, Id}) ->  % {{{1
    wf:session(current_task_id, Id),
    EID = wf:to_atom(binary:decode_unsigned(Id)),
    Right = wf:session(right_parent_id),
    {ok, [ #db_task{parent=Par, status=S} = Task ]} = db:get_task(Id),
    wf:session(current_task, Task),
    wf:session(current_task_id, Id),
    if Id /= Right, Par /= Right ->  % {{{1
           wf:session(right_parent_id, Id);
       true ->  % {{{1
           ok
    end,
    render_tasks(),
    wf:update(body, render_task(Task));
event({task_list, prrev}) ->  % {{{1
    Left = wf:session(left_parent_id),
    case db:get_task(Left) of
        {ok,  [ #db_task{id=Left, parent=Parent} ] } ->  % {{{1
            wf:session(left_parent_id, Parent),
            wf:session(right_parent_id, Left),
            render_tasks();
        {ok, []} ->  % {{{1
            ok
    end;
event({task_list, next}) ->  % {{{1
    Id = wf:session(current_task_id),
    Right = wf:session(right_parent_id),
    wf:session(left_parent_id, Right),
    wf:session(right_parent_id, Id),
    render_tasks();
event({edit, Id}) ->  % {{{1
    Task = wf:session(current_task),
    wf:session(current_task, Task#db_task{status=changed}),
    wf:redirect("/edit_task");
event(hide) ->  % {{{1
    wf:wire(body, [#remove_class{class="span8"}, #add_class{class="span12"}]),
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link", body="Show tasks <i class='icon-angle-right'></i>", 
                                    actions=#event{type=click, actions=[
                                        #show{trigger=hide_show,target=tasks}, 
                                        #event{postback=show}
                                        ]}});
event(show) ->  % {{{1
    wf:wire(body, [#remove_class{class="span12"}, #add_class{class="span8"}]),
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link", body="<i class='icon-angle-left'></i> Hide tasks", 
                                    actions=#event{type=click, actions=[
                                        #hide{trigger=hide_show,target=tasks}, 
                                        #event{postback=hide}
                                        ]}});
event(Click) ->  % {{{1
    io:format("~p~n",[Click]).

drop_event({task, Id}, { subtask, PId }) when PId /= Id->  % {{{1
    case db:get_task(PId) of 
        {ok, [#db_task{parent=Id}]} ->  % {{{1
            ok;
        _ ->  % {{{1
            db:save_subtask(Id, PId),
            %db:save_task_tree(Id, PId),
            common:send_task_tree(Id, PId),
            wf:wire(#event{postback={task_chosen, PId}})
    end;
drop_event({task, Id}, task) ->  % {{{1
    PId = wf:session(left_parent_id),
    common:send_task_tree(Id, PId),
    %db:delete_task_tree(Id, PId),
    db:save_subtask(Id, PId);
drop_event(_, _) ->  % {{{1
    ok.

incoming() ->  % {{{1
    render_tasks(),
    wf:flush().
