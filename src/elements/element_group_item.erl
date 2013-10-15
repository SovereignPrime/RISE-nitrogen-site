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
    #listitem{class="clearfix", body=[
            #droppable{tag={subgroup, Id}, accept_groups=[groups, contacts], body=
                    #draggable{tag={group, Id}, group=groups, body=
                               #inplace_textbox{tag={group, Id}, class="pull-left", text=Name}
                              }},
                    case Sub of
                        [] ->
                            "";
                        _ ->
                            [" <i class='icon-caret-left clearfix'></i>", 
                             #list{numbered=false,style='margin-left:15px;',
                                   body=[

                                        lists:map(fun(#db_group{id=I, name=N, subgroups=S}) ->
                                                    #group_item{gid=I, name=N, sub=S}
                                            end, Sub)
                                        ]}
                            ]
                    end
                    
            ]}.

