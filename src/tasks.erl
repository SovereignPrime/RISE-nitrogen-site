%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (tasks).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").

-define(UPDATE_CURRENT(Field, Val),
            update_current_task(fun(T) ->
                T#db_task{Field=Val}
            end)
       ).

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
    case wf:session(filter) of
        undefined ->
            wf:session(task_tree_mode, task_tree),
            #panel{id=tasks, class="span4 scrollable", body=[
                                                             render_task_tree()
                                                            ]};
        D ->
            wf:session(task_tree_mode, filter),
            #panel{id=tasks, class="span4 scrollable", body=[
                                                             render_task_tree()
                                                            ]}
    end.


render_task_tree_buttons(Selected) ->  % {{{1
    Buttons = [
    %%  { Label, postback, width }
        {"Tree", task_tree, 2},
        {"Today", tasks_today, 2},
        {"Next", tasks_soon, 2},
        {"No Deadline", tasks_no_deadline, 3},
        {"Complete", tasks_complete, 3}
    ],
    lists:map(fun({Label, Postback, Size}) ->
        SizeClass = wf:to_atom(["span",wf:to_list(Size)]),
        #link{
           class=[SizeClass, 'task-tree-button', ?WF_IF(Postback==Selected, 'task-tree-button-selected', 'task-tree-button-unselected')],
           text=Label,
           postback={change_mode, Postback}
        }
    end, Buttons).

update_task_tree() ->  % {{{1
    update_task_tree(false).

update_task_tree(Archive) ->  % {{{1
    wf:update(tasks, render_task_tree(Archive)).

render_task_tree() ->  % {{{1
    render_task_tree(false).

render_task_tree(Archive) ->  % {{{1
    Mode = wf:session_default(task_tree_mode, task_tree),
    Buttons = render_task_tree_buttons(Mode),
    Tree = case Mode of
        task_tree ->
            render_task_tree(undefined, Archive, true);
        _ ->
            render_task_list(Mode, Archive)
    end,
    highlight_selected(),
    [Buttons, Tree].

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
    MD5 = crypto:hash(sha, Data),
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= MD5]).

render_subtask(Task = #db_task{name=Name, status=Status, due=Due, id=Id}, Archive) ->  % {{{1
    case Status==complete andalso db:are_all_child_tasks_complete(Id) of
        true -> [];
        false ->
            ThisTaskIdMd5 = md5(Id),
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
                            render_task_link(Task)
                       ]}
                    ]}
                ]},
                Subtree
            ]}
    end.

render_task_link(Task = #db_task{name=Name, due=Due, id=Id}) ->
    HasAttachments = does_task_have_attachments(Task),
    render_task_link(Id, Name, HasAttachments, Due).

render_task_link(Id, Name, HasAttachments, Due) ->
    ThisTaskIdMd5 = md5(Id),
    [
        #link{postback={task_chosen, Id}, data_fields=[{link, ThisTaskIdMd5}], body=[
            #image{style="width:16px; height:16px", image="/img/tasks.svg"},
            #span{text=Name},
            "&nbsp;",
            ?WF_IF(HasAttachments, "<i title='This task has files attached' class='icon-paperclip'></i>")

        ]},
        #span{style="font-size:0.8em; white-space:nowrap",body=[
            " (",
            ?WF_IF(Due,["Due: ",Due],"No due date"),
            ")",
            "&nbsp;",
            #link{body="<i class='icon-plus' style='font-size:10px'></i>",
                  title="Add New Sub-Task",
                  postback={add, Id}
            }
        ]}
    ].

does_task_have_attachments(Task) ->
    {ok, Attachments} = db:get_attachments(Task),
    _HasAttachments = length(Attachments) >= 1.

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

render_task_list(Mode, Archive) ->  % {{{1
    Function = case Mode of
        tasks_today -> fun db:get_tasks_due_today/1;
        tasks_soon -> fun ?MODULE:get_next_tasks_by_date/1;
        tasks_no_deadline -> fun db:get_tasks_no_deadline/1;
        filter -> fun ?MODULE:get_tasks_by_filter/1;
        tasks_complete -> fun db:get_tasks_completed/1
    end,
    {ok, Tasks} = Function(Archive),
    #list{
       numbered=false,
       data_fields=[{list, md5(undefined)}],
       style=["padding-left: 10px; "],
       body=[render_flat_task(T, Archive) || T <- Tasks]
    }.

get_tasks_by_filter(_) ->
    Filter = wf:session(filter),
    db:search_tasks(Filter).

get_next_tasks_by_date(Archive) -> % {{{1
    Today = sugar:date_format(date()),
    {ok, lists:filter(fun(#db_task{status=complete}) ->
                              false;
                         (_) ->
                              true
                      end, lists:dropwhile(fun(#db_task{due=""}) ->
                                                   true;
                                              (#db_task{due=D}) when D < Today ->
                                                   true;
                                              (_) ->
                                                   false
                                           end, get_tasks_sorted_by_date(Archive)))}.

get_tasks_sorted_by_date(Archive) ->  % {{{1
    {ok, Tasks} = db:get_tasks(Archive),
    lists:sort(fun compare_task_date/2, Tasks).

compare_task_date(#db_task{due=""}, _) ->  % {{{1
    true;
compare_task_date(_, #db_task{due=""}) ->  % {{{1
    false;
compare_task_date(#db_task{due=A}, #db_task{due=B}) ->  % {{{1
    A =< B.

render_flat_task(Task, Archive) ->  % {{{1
    #listitem{body=render_task_link(Task)}.

body() ->  % {{{1
    case wf:session(current_task) of
        #db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}=Task -> 
            wf:state(current_task, Task),
            wf:state(current_task_id, Id),
            highlight_selected(Id),
            #panel{id=body, class="span8 scrollable", body=
                   [
                    render_task(Task)
                   ]};
        undefined ->
            #panel{id=body, class="span8", body=[]}
    end.


render_task(#db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status, changes=Changes}=Task) ->  % {{{1
    TextF = re:replace(Text, "\r*\n", "<br>", [{return, list}, noteol, global]), 
    {ok, Updates} = db:get_task_history(Id),
    AllComplete = db:are_all_child_tasks_complete(Id),
    IncompleteWarning = ?WF_IF(Status==complete andalso not(AllComplete), "<i class='icon-exclamation-sign' title=\"Task marked complete but has incomplete subtasks\"></i>"),
    StatusDropdown = #dropdown{options=db:task_status_list(), value=Status},
    [
        render_top_buttons(),
        #panel{ class="row-fluid", body=[
            #panel{ class="span11", body=[
                #h1{body=#inplace_textbox{id=name, tag=name, text=Name}},
                #panel{class="row-fluid", style="min-height:15px;", body=[
                     #panel{ class="span2", style="min-height:15px;", body=["Status: ", IncompleteWarning]},
                     #inplace{id=status, 
                              style="min-height:15px;",
                              class="span6",
                              tag=status,
                              text=wf:to_list(Status),
                              view=#span{},
                              edit=StatusDropdown
                     }
                ]},
                #panel{class="row-fluid", body=[
                    #panel{ class="span2", body="Due: "},
                    #inplace{id=due,
                             style="min-height:15px;",
                             class="span6",
                             tag=due,
                             text=Due,
                             view=#span{},
                             start_mode=view,
                             edit=#datepicker_textbox{text=Due}
                    }
                ]},
                render_roles(Id)
            ]},
            #panel{ class="span1", body=render_side_buttons(Id, Task)}
        ]},
        #br{},
        #panel{ class="row-fluid", body=[
            #panel{ class="span10", body=[
                #inplace_textarea{id=text,
                                  class="span12",
                                  tag=text,
                                  html_encode=whites,
                                  text=Text}
            ]}
        ]},
        render_attachments(Task),
        render_updates(Updates),
        render_task_changes(Changes)
    ]. 

get_involved_full() -> % {{{1
    Id = wf:state(current_task_id),
    get_involved_full(Id).

get_involved_full(Id) -> % {{{1
    case wf:state(involved) of
        undefined ->
            {ok, Inv} = db:get_involved_full(Id),
            wf:state(involved, Inv),
            {ok, Inv};
        Inv ->
            {ok, Inv}
    end.


render_roles(Id) -> % {{{1
    {ok, Involved} = get_involved_full(Id),
    [
        #panel{id=role_wrapper, class="row-fluid", body=[
            [render_role_row(Inv) || Inv <- Involved]
        ]},
        #panel{class="row-fluid", body=[
            #button{class="btn btn-link", body="<i class='icon-plus'></i> Add Role", postback=add_role}
        ]}
    ].

render_role_row({ContactRole, Name}) -> % {{{1
    Rowid = wf:temp_id(),
    Edit = #event{type=click, postback={edit_role, Rowid, {ContactRole, Name} }},
    #panel{id=Rowid, class="row-fluid role-row", actions=Edit, body=[
        #panel{class="span2", body=[ContactRole#db_contact_roles.role,":"]},
        #panel{class="span6", body=Name},
        #panel{class="span4", style="background-color: #fff", body=""}
    ]}.

render_role_edit_row(OriginalData = {ContactRole, Name}) -> % {{{1
    Rowid = wf:temp_id(),
    RoleFieldid = wf:temp_id(),
    NameFieldid = wf:temp_id(),
    #panel{id=Rowid, class="row-fluid", body=[
        #panel{class=span2, body=[
            element_involved:role_dropdown(RoleFieldid, ContactRole#db_contact_roles.role)
        ]},
        #panel{class=span4, body=[
            #textbox_autocomplete{id=NameFieldid, tag=contact, class="span11", text=Name, delegate=common}
        ]},
        #panel{class=span2, body=[
            #button{class="btn btn-link", body="<i class='icon-ok'></i>", postback={save_role, Rowid, OriginalData, RoleFieldid, NameFieldid}},
            #button{class="btn btn-link", body="<i class='icon-remove'></i>", postback={cancel_role, Rowid, OriginalData}}
        ]}
    ]}.


render_top_buttons() -> % {{{1
    #panel{ id=top_buttons,
            class="row-fluid",
            style="display:none",
            body=[
                  #panel{ class="span4 offset4",
                          body=[
                                #panel{class="row-fluid",
                                       body=[
                                             #button{ class="btn btn-link span6",
                                                      body="<i class='icon-remove'></i> Discard",
                                                      postback=discard},

                                             #button{ class="btn btn-link span6",
                                                      body="<i class='icon-ok'></i> Save",
                                                      postback=save}
                                            ]}
                               ]}
                 ]}.

render_side_buttons(Id, Task) -> % {{{1
    [
        #panel{class="btn btn-link", body = [
            #link{postback={edit, Id}, new=false, body=[
                "<i class='icon-edit icon-large'></i><br>"      
            ]}
        ]},
        #br{},
        #panel{class="btn-group", body=[
            #link{new=false,
                  data_fields=[{toggle, "dropdown"}],
                  class="btn btn-link droppdown-toggle",
                  body="<i class='icon-reorder icon-large'></i>"
            },
            #list{numbered=false, class="dropdown-menu pull-right", body=[
                #listitem{body=[
                    #link{postback={archive, Task}, new=false, body=[
                        "<i class='icon-list-alt icon-large'></i> Archive"
                    ]}
                ]}
            ]}
        ]}
    ].

render_attachments(Task) ->
    case db:get_attachments(Task) of 
        {ok, []} ->
            [];
        {ok, [], undefined} ->
            [];
        {ok, Attachments} ->
            [
                #br{},
                #panel{class="row-fluid", body=[
                    #panel{class="span6", body="<i class='icon-file-alt'></i> Attachment"},
                    #panel{class="span2 offset4", body="<i class='icon-download-alt'></i> Download all"}
                ]},
                lists:map(fun(#bm_file{name=Path, size=Size, time={Date, _Time}, hash=Id, status=State}) ->
                    #attachment{fid=Id, filename=Path, size=Size, time=Date, status=State}
                end, Attachments)
            ]
    end.

render_updates([]) -> [];
render_updates(Updates) ->
    [
        #br{},
        #panel{class="row-fluid", body=[
            #panel{class="span6", body="<i class='icon-envelope'></i> Related Messages"}
        ]},
        [#update_element{
           collapse=true,
           message=M} || M <- sugar:sort_by_timestamp(Updates)]
    ].

render_task_changes([]) -> [];
render_task_changes(Changes) ->
    [
        #br{},
        #panel{class="row-fluid", body=[
            #panel{class="span12", body="<i class='icon-time'></i> Change History"}
        ]},
        [render_task_change(C) || C <- Changes]
    ].

render_task_change(C) ->
    Contact = case db:get_contact_by_address(C#db_task_change.address) of
                  none -> "Anonymous";
                  {ok, Co} -> Co#db_contact.name
              end,
    {Date, _} = C#db_task_change.datetime,
    #panel{class="row-fluid", body=[
        #panel{class="span2", text=sugar:date_format(Date)},
        #panel{class="span2", text=Contact},
        #panel{class="span6", text=["changed ",C#db_task_change.field," to ",C#db_task_change.new_value]}
    ]}.

highlight_selected() ->
    case wf:session(current_task) of
        #db_task{id=Id} -> highlight_selected(Id);
        _ -> ok
    end.

highlight_selected(Id) ->
    Md5 = md5(Id),
    wf:defer(#remove_class{target=".wfid_tasks a", class=current}),
    wf:defer(#add_class{target=".wfid_tasks a[data-link=\"" ++ Md5 ++ "\"]", class=current}).

check_changing_task_status() -> ok.

save_contact_role(CR = #db_contact_roles{id=new}) -> % {{{1
    Taskid = wf:state(current_task_id),
    {ok, Id} = db:next_id(db_contact_roles),
    NewCR = CR#db_contact_roles{
              id=Id,
              type=db_task,
              tid=Taskid},
    save_contact_role(NewCR);
save_contact_role(CR) -> % {{{1
    db:save(CR).

event({change_mode, Mode}) ->
    wf:session(task_tree_mode, Mode),
    update_task_tree();
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
    Right = wf:session(right_parent_id),
    {ok, [ #db_task{parent=Par, status=S} = Task ]} = db:get_task(Id),
    wf:session(current_task_id, Id),
    wf:session(current_task, Task),
    wf:state(involved, undefined),
    wf:state(current_task, Task),
    wf:state(current_task_id, Id),
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
event(save) -> % {{{1
    Task = wf:state(current_task),
    Involved = wf:state(involved),
    Task2 = calculate_changes(Task),
    db:save(Task2),
    [save_contact_role(ContactRole) || {ContactRole, _} <- Involved],
    common:send_messages(Task2),
    update_task_tree(),
    event({task_chosen, Task#db_task.id});
event(discard) -> % {{{1
    Task = wf:state(current_task),
    event({task_chosen, Task#db_task.id});
    
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

event(add_role) -> % {{{1
    wf:insert_bottom(role_wrapper, render_role_edit_row({#db_contact_roles{id=new}, ""}));

event({edit_role, Rowid, OriginalData}) -> % {{{1
    wf:replace(Rowid, render_role_edit_row(OriginalData));

event({cancel_role, Rowid, OriginalData}) -> % {{{1
    case OriginalData of
        {#db_contact_roles{id=new}, _} ->
            wf:remove(Rowid);
        _ ->
            wf:replace(Rowid, render_role_row(OriginalData))
    end;

event({save_role, Rowid, {OrigContactRole, _OrigName} = OriginalData, RoleFieldid, NameFieldid}) -> % {{{1
    Name = wf:q(NameFieldid),
    Role = wf:q(RoleFieldid),
    {ok, #db_contact{id=Contactid}} = db:get_contacts_by_name(Name),
    ContactRole = OrigContactRole#db_contact_roles{contact=Contactid, role=Role},
    {ok, CurrentInvolved} = get_involved_full(),
    %% In-place editing of the lists contents since there isn't a lists:replace function
    NewInvolved = case lists:member(OriginalData, CurrentInvolved) of
        true ->
            lists:map(fun(Data) ->
                        case Data of
                                OriginalData -> {ContactRole, Name};
                                _ -> Data
                        end
            end, CurrentInvolved);
        false ->
            CurrentInvolved ++ [{ContactRole, Name}]
    end,
    wf:state(involved, NewInvolved),
    maybe_show_top_buttons(),
    wf:replace(Rowid, render_role_row({ContactRole, Name}));

event(Click) ->  % {{{1
    io:format("~p~n",[Click]).



inplace_textarea_event(text, Val) -> % {{{1
    ?UPDATE_CURRENT(text, wf:to_binary(Val)),
    Val.

inplace_textbox_event(name, Val) -> % {{{1
    ?UPDATE_CURRENT(name, wf:to_binary(Val)),
    Val.

inplace_event(status, Val) ->  % {{{1
    NewStatus = db:sanitize_task_status(Val),
    Taskid = wf:state(current_task_id),
    Acceptable = NewStatus=/=complete orelse db:are_all_child_tasks_complete(Taskid),
    case Acceptable of
        true ->
            ?UPDATE_CURRENT(status, db:sanitize_task_status(Val)),
            Val;
        false -> 
            Task = wf:state(current_task),
            Status = Task#db_task.status,
            wf:wire(#alert{text="Sorry, you can not mark a task as complete until all its subtasks are also complete"}),
            Status
    end;

inplace_event(due, Val) -> % {{{1
    ?UPDATE_CURRENT(due, Val),
    Val;
inplace_event(_, V) ->  % {{{1
    V.

update_current_task(Fun) -> % {{{1
    Task = wf:state(current_task),
    NewTask = Fun(Task),
    maybe_show_top_buttons(NewTask),
    wf:state(current_task, NewTask).

maybe_show_top_buttons() -> % {{{1
    CurrentTask = wf:state(current_task),
    maybe_show_top_buttons(CurrentTask).

maybe_show_top_buttons(CurrentTask) -> % {{{1
    Taskid = wf:state(current_task_id),
    {ok, [TaskFromDB]} = db:get_task(Taskid),
    
    {ok, InvolvedFromDB} = db:get_involved_full(Taskid),
    NewInvolved = wf:state(involved),
   

    TaskChanged = TaskFromDB =/= CurrentTask,
    InvolvedChanged = InvolvedFromDB =/= NewInvolved,

    case TaskChanged orelse InvolvedChanged of
        true -> wf:wire(top_buttons, #show{});
        false -> wf:wire(top_buttons, #hide{})
    end.

calculate_changes(Task) -> % {{{1
    Id = Task#db_task.id,
    {ok, [TaskFromDB]} = db:get_task(Id),
    Fields = record_info(fields, db_task),
    #db_contact{address=Me} = wf:user(),
    OriginalChanges = Task#db_task.changes,
    NewChanges = lists:foldl(fun ({_, changes}, Acc) -> Acc;
                                 ({_, id}, Acc) -> Acc;
                                 ({Fieldnum, Field}, Acc) ->
        case element(Fieldnum+1, Task) =:= element(Fieldnum+1, TaskFromDB) of
            true -> Acc;
            false ->
                FieldValue = element(Fieldnum+1, Task),
                NewValue = string:strip(lists:flatten(io_lib:format("~100s",[FieldValue]))),
                IsShortened = wf:to_list(NewValue) =/= wf:to_list(FieldValue),
                NewValue2 = ?WF_IF(IsShortened, NewValue ++ "...", NewValue),
                Change = #db_task_change{
                            address=Me,
                            datetime=calendar:local_time(),
                            field=Field,
                            new_value=NewValue2
                         },
                [Change | Acc]
        end
    end, OriginalChanges, lists:zip(lists:seq(1, length(Fields)),Fields)),

    Involved = wf:state(involved),
    {ok, OriginalInvolved} = db:get_involved_full(Id),

    NewChanges2 = case Involved =:= OriginalInvolved of
                      true -> NewChanges;
                      false -> [#db_task_change{
                                   address=Me,
                                   datetime=calendar:local_time(), 
                                   field=involved,
                                   new_value=summarize_involved()
                                } | NewChanges]
                  end,
    Task#db_task{changes=NewChanges2}.

summarize_involved() -> % {{{1
    Involved = wf:state(involved),
    InvolvedStrings = [(wf:to_list(CR#db_contact_roles.role) ++ ": " ++ wf:to_list(Name)) 
                       || {CR, Name} <- Involved],
    string:join(InvolvedStrings, ", ").


drop_event({task, Id}, { subtask, PId }) when PId =:= Id->  % {{{1
    ok;

drop_event({task, Id}, { subtask, PId }) when PId /= Id->  % {{{1
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
    wf:update(tasks, render_task_tree()),
    wf:flush().
