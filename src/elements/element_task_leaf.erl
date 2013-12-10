%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_task_leaf).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, task_leaf).

-spec render_element(#task_leaf{}) -> body().
render_element(_Record = #task_leaf{tid=Id, due=Due, name=Task, delegate=Delegate, checked=Checked, current=false}) ->
    #draggable{tag={task, Id}, group=tasks, clone=false, body=[
            #listitem{class="clearfix", body=[
                    #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback={check, Id}, checked=Checked, delegate=Delegate},
                    "<b>", Task, "</b>", #br{},
                    "Due: ",  Due
                    ], actions=#event{type=click, postback={task_chosen, Id}, delegate=Delegate}}]};
render_element(_Record = #task_leaf{tid=Id, due=Due, name=Task, delegate=Delegate, checked=Checked, current=true}) ->
    #draggable{tag={task, Id}, group=tasks, clone=false, body=[
            #droppable{id=task, tag=subtask, accept_groups=tasks, body=[
                    #listitem{class="clearfix current", body=[
                            #panel{class="pull-right", style="margin: 10px 15px 0;", body="<i class='icon-chevron-right icon-large'></i>"},
                            #checkbox{id=john, class="pull-left", style="margin: 10px 15px 0;", postback={check, Id}, checked=Checked, delegate=Delegate},
                            "<b>", Task, "</b>", #br{},
                            "Due: ", Due
                            ]}
                    ], actions=#event{type=click, postback={task_chosen, Id}, delegate=Delegate}}]}.
