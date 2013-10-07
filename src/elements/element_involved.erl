%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_involved).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, involved).

-spec render_element(#involved{}) -> body().
render_element(_Record = #involved{id=Id, num=N}) ->
            #panel{ class="row-fluid", body=[
                    #panel{ class="span9", body=[
                            #span{ class="add-on", body=[
                                    #span{html_encode=false, text="<i class='icon-user'></i>"}
                                    ]},
                    #textbox{id=wf:f("~s_person~p", [Id, N]), text="People", next=wf:f("i~s_responsible~p", [Id, N]), class="span11"}
                            ]},
                    #panel{class="dropdown span3", body=[
                            #dropdown{id=wf:f("~s_responsible~p", [Id, N]), value="", class="span12", options=[
                                    #option{text="Responsible", value="responsible"},
                                    #option{text="Accountable", value="accountable"}
                                    ]}

                            ]}
                    ]}.
