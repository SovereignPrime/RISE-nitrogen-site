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
    #list{numbered=false, class="nav nav-pills", style="display:inline-block", body=[
        #listitem{body=[
            %#panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
        ]},
        #listitem{body=[
            #button{
                id=hide_show,
                class="btn btn-link",
                body="<i class='icon-angle-left'></i> Hide tasks",
                click=[
                    #hide{trigger=hide_show,target=tasks}, 
                    #event{postback=hide}
                ]
            }
       ]},
        #listitem{body=[
            common:render_filters()
        ]},
        %#listitem{body=[
        %    #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
        %]},
        #listitem{body=[
            #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}
        ]},
        #listitem{body=[
            common:settings_menu()
        ]},
        #listitem{body=[
            common:render_help()
        ]}
    ]}.

left() ->  % {{{1
    CId = wf:session(current_task_id),
    #panel{id=tasks, class="span4", body=[
            render_task_tree()
    ]}.

update_task_tree() ->  % {{{1
    update_task_tree(false).

update_task_tree(Archive) ->  % {{{1
    wf:update(tasks, render_task_tree(Archive)).

render_task_tree() ->  % {{{1
    render_task_tree(false).

render_task_tree(Archive) ->  % {{{1
    render_task_tree(undefined, Archive, true).

render_task_tree(ParentId, Archive, First) ->  % {{{1
    Body = case db:get_tasks(ParentId, Archive) of
        {ok, []} ->
             [];
        {ok, Tasks} ->
            #list{
               id=wf:temp_id(),
               numbered=false,
               data_fields=[{list, md5(ParentId)}],
               style=["padding-left: 10px; "],
               body=[render_subtask(T, Archive) || T <- Tasks]
            };
        {ok, [], undefined} ->
             []
    end,
    case First of
        true ->
            #droppable{tag=task_root, accept_groups=[task_groups], style="", body=[
                #panel{body=["Tasks",Body]}
            ]};
        false ->
            Body
    end.

md5(undefined) ->  % {{{1
    md5("");
md5(Data) ->  % {{{1
    MD5 = crypto:hash(md5, Data),
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= MD5]).

render_subtask(Task = #db_task{name=Name, due=Due, id=Id}, Archive) ->  % {{{1
    ThisTaskIdMd5 = md5(Id),
    {ok, Attachments} = db:get_attachments(Task),
    HasAttachments = length(Attachments) >= 1,
    {Expander, Subtree} = case render_task_tree(Id, Archive, false) of
        [] -> {#span{style="width:10px;display:inline-block"}, []};
        Tree -> 
            Sublistid = Tree#list.id,
            {#expander{target=Sublistid, data_fields=[{parent,ThisTaskIdMd5}], start=open},Tree}
    end,
    
    #listitem{body=[
        #droppable{tag={subtask, Id}, accept_groups=[task_groups], body=[
            #draggable{tag={task, Id}, group=task_groups, clone=false, distance=20, options=[{delay, 300}], body=[
                #panel{style="display:block", body=[
                    Expander,
                    #link{postback={task_chosen, Id}, data_fields=[{link, ThisTaskIdMd5}], body=[
                        #image{style="width:16px; height:16px", image="/img/tasks.svg"},
                        #span{text=Name},
                        "&nbsp;",
                        ?WF_IF(HasAttachments, "<i title='This task has files attached' class='icon-paperclip'></i>"),

                        #span{style="font-size:0.8em",body=[" (",?WF_IF(Due,["Due: ",Due],"No due date"),")"]}
                    ]},
                    "&nbsp;",
                    #link{body="<i class='icon-plus'></i>", title="Add New Sub-Task", postback={add, Id}}
               ]}
            ]}
        ]},
        Subtree
    ]}.
 
render_tasks() ->  % {{{1
    render_task_tree().

expand_to_task(Taskid) ->  % {{{1
    case db:get_task(Taskid) of
        {ok, [#db_task{parent=undefined}]} ->
            ok;
        {ok, [#db_task{parent=Parentid}]} ->
            expand_task(Parentid),
            expand_to_task(Parentid);
        _ ->
            ok
    end.

expand_task(Taskid) ->  % {{{1
    Hashed = md5(Taskid),
    wf:wire(["$(\".expander[data-parent='",Hashed,"']\").addClass('icon-caret-down').removeClass('icon-caret-right')"]),
    wf:wire(["$(\".list[data-list='",Hashed,"']\").show();"]).

body() ->  % {{{1
    case wf:session(current_task) of
        #db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}=Task -> 
            #panel{id=body, class="span8", body=
                   [
                    render_task(Task)
                   ]};
        undefined ->
            #panel{id=body, class="span8", body=[]}
    end.


render_task(#db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}=Task) ->  % {{{1
    {ok, Involved} = db:get_involved(Id),
    {My, InvolvedN} = case lists:partition(fun({_, _, #db_contact{my=true}}) -> true; (_) -> false end, Involved) of 
        {[{_,M, _}|_], I} ->  
            {M, I};
        {[], I} ->  
            {no, I}
    end, 
    TextF = re:replace(Text, "\r*\n", "<br>", [{return, list}, noteol, global]), 
    {ok, Updates} = db:get_task_history(Id),
    Dropdown = #dropdown{options=db:task_status_list(), value=Status},
    [
        #panel{ class="row-fluid", body=[
                #panel{ class="span11", body=[
                        #h1{text=Name},
                        #panel{class="row-fluid", 
                               style="min-height:15px;",
                               body=[
                                     #panel{ class="span1", 
                                             style="min-height:15px;",
                                             body="Status: "},

                                     #inplace{id=status, 
                                              style="min-height:15px;",
                                              class="span10",
                                              tag=status,
                                              text=wf:to_list(Status),
                                              view=#span{},
                                              edit=Dropdown
                                             }
                                    ]},
                        #panel{class="row-fluid", 
                               body=[
                                     #panel{ class="span1", body="Due: "},
                                     #panel{ class="span3", body=Due}
                                     ]},
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
        #br{},
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
        #br{},
        [
         #update_element{collapse=true, from=From, to=To, text=Text, uid=Id, subject=Subject, enc=Enc, status=Status} || #message{hash=Id, enc=Enc, to=To, subject=Subject, from=From, text=Text, status=Status} <- sugar:sort_by_timestamp(Updates)
        ] 
    ]. 

highlight_selected(Id) ->
    Md5 = md5(Id),
    wf:wire(#remove_class{target=".wfid_tasks a", class=current}),
    wf:wire(#add_class{target=".wfid_tasks a[data-link=\"" ++ Md5 ++ "\"]", class=current}).

event({archive, #db_task{id=_Id, parent=_Parent} = Rec}) ->  % {{{1
    {ok, NTask} = db:archive(Rec),
    common:send_messages(NTask),
    update_task_tree(),
    wf:update(body, render_task(Rec));
event({show_archive, true}) ->  % {{{1
    update_task_tree(),
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    wf:update(subgroups, []);
event({show_archive, false}) ->  % {{{1
    update_task_tree(false),
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    wf:update(subgroups, []);
event({task_chosen, Id}) ->  % {{{1
    wf:session(current_task_id, Id),
    Right = wf:session(right_parent_id),
    {ok, [ #db_task{parent=Par, status=S} = Task ]} = db:get_task(Id),
    wf:session(current_task, Task),
    wf:session(current_task_id, Id),
    wf:update(body, render_task(Task)),
    expand_task(Id),
    highlight_selected(Id);
event({add, ParentId}) -> % {{{1
    wf:session(current_task, #db_task{parent=ParentId}),
    wf:redirect("/edit_task");
event({edit, Id}) ->  % {{{1
    Task = wf:session(current_task),
    wf:session(current_task, Task),
    wf:redirect("/edit_task");
event(hide) ->  % {{{1
    wf:wire(body, [#remove_class{class="span8"}, #add_class{class="span12"}]),
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link", body="Show task list <i class='icon-angle-right'></i>", 
                                    actions=#event{type=click, actions=[
                                        #show{trigger=hide_show,target=tasks}, 
                                        #event{postback=show}
                                        ]}});
event(show) ->  % {{{1
    wf:wire(body, [#remove_class{class="span12"}, #add_class{class="span8"}]),
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link", body="<i class='icon-angle-left'></i> Hide task list", 
                                    actions=#event{type=click, actions=[
                                        #hide{trigger=hide_show,target=tasks}, 
                                        #event{postback=hide}
                                        ]}});
event(Click) ->  % {{{1
    io:format("~p~n",[Click]).

inplace_event(status, Val) ->  % {{{1
    Task = wf:session(current_task),
    NTask = Task#db_task{status=db:sanitize_task_status(Val)},
    wf:session(current_task, NTask),
    db:save(NTask),
    Val;

inplace_event(_, V) ->  % {{{1
    V.

drop_event({task, Id}, { subtask, PId }) when PId =:= Id->  % {{{1
    ok;

drop_event({task, Id}, { subtask, PId }) when PId /= Id->  % {{{1
    error_logger:info_msg("Taskid: ~p~nSubtask: ~p",[Id, PId]),
    case db:get_task(PId) of
        {ok, [#db_task{parent=Id}]} ->
            ok;
        _ ->
            case db:get_task(Id) of
                {ok, [#db_task{parent=PId}]} ->
                    ok;
                _ ->
                    db:save_subtask(Id, PId, bm_types:timestamp()),
                    common:send_task_tree(Id, PId, bm_types:timestamp()),
                    update_task_tree(),
                    expand_to_task(Id),
                    event({task_chosen, Id})
            end
    end;

drop_event({task, Id}, task_root) ->  % {{{1
    PId = wf:session(left_parent_id),
    db:save_subtask(Id, PId, bm_types:timestamp()),
    common:send_task_tree(Id, PId, bm_types:timestamp()),
    update_task_tree().

incoming() ->  % {{{1
    wf:update(tasks, render_tasks()),
    wf:flush().
