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
                                           #listitem{class="clearfix", style="margin-bottom: 10px;", body=[
                                                                     #panel{ class="row-fluid", body=[
                                                                                                      #panel{ class="span1", body=[

                                                                                                                                   #checkbox{id=john, postback={check, Id}, checked=Checked, delegate=Delegate}
                                                                                                                                  ]},
                                                                                                      #panel{ class="span11", body=[

                                                                                                                                    "<b class='shorten-text' style='-webkit-line-clamp:1;'>", Task, "</b>",
                                                                                                                                    "Due: ",  Due
                                                                                                                                   ]}
                                                                                                     ]}
                                                                            ], actions=#event{type=click, postback={task_chosen, Id}, delegate=Delegate}}]};
render_element(_Record = #task_leaf{tid=Id, due=Due, name=Task, delegate=Delegate, checked=Checked, current=true}) ->
    #draggable{tag={task, Id}, group=tasks, clone=false, body=[
            #droppable{id=task, tag=subtask, accept_groups=tasks, body=[
                    #listitem{class="clearfix current", style="margin-bottom: 10px;", body=[
                                                                     #panel{ class="row-fluid", body=[
                                                                                                      #panel{ class="span1", body=[

                                                                                                                                   #checkbox{id=john, postback={check, Id}, checked=Checked, delegate=Delegate}
                                                                                                                                  ]},
                                                                                                      #panel{ class="span11", body=[
                                                                                                                                    "<b class='shorten-text'  style='-webkit-line-clamp:1;'>", Task, "</b>",
                                                                                                                                    "Due: ",  Due
                                                                                                                                   ]}
                                                                                                     ]}
                            ]}
                    ], actions=#event{type=click, postback={task_chosen, Id}, delegate=Delegate}}]}.
