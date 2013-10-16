%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_group_item).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, group_item).

-spec render_element(#group_item{}) -> body().
render_element(_Record = #group_item{gid=Id, name=Name, sub=Sub}) ->
    #listitem{class="", body=[
            #draggable{tag={group, Id}, group=groups, clone=false, body=
                       case Sub of
                    [] ->
                        #droppable{tag={subgroup, Id}, accept_groups=[groups, contacts], body=[
                                   #span{text=Name, actions=[
                                    #event{type=click, postback={group, Id}},
                                    #event{type=hover, actions=#show{}}
                                    ]},
                                #span{class="",  body=[
                                        #link{class="btn btn-link dropdown-toggle",data_fields=[{toggle, "dropdown"}], body=[
                                                "<i class='icon-tasks'></i>"
                                                   ], url="#", new=false},
                                        #list{numbered=false, class="dropdown-menu",
                                            body=[
                                                #listitem{body=[
                                                        #link{body=[
                                                        "<i class='icon-trash'></i> Delete"
                                                        ], url="", new=false}
                                                              ]},
                                                #listitem{body=[
                                                        #link{body=[
                                                        "<i class='icon-trash'></i> Delete"
                                                        ], url="", new=false}
                                                              ]}
                                        ]}
                                        ]}
                                ]};
                    _ ->
                        [
                            #droppable{tag={subgroup, Id}, accept_groups=[groups, contacts], body=[
                                    #span{text=Name, actions=#event{type=click, postback={group, Id}}},
                                    " <i class='icon-caret-down'></i>"]},
                            #list{numbered=false,style='margin-left:15px;',
                                  body=[

                                    lists:map(fun(#db_group{id=I, name=N, subgroups=S}) ->
                                                #group_item{gid=I, name=N, sub=S}
                                        end, Sub)
                                    ]}
                            ]
                end
                      }]}.
event(E) ->
    io:format("Event ~p in ~p~n", [E, ?MODULE]).
