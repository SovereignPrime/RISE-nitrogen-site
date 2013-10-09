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
render_element(_Record = #involved{}) ->
    {ok, Users, _} = db:get_users(10), % Move from here
    #panel{ class="row-fluid", body=[
            #panel{ class="span9", body=[
                    #span{ class="add-on", body=[
                            #span{html_encode=false, text="<i class='icon-user'></i>"}
                            ]},
                    #textbox{id=person, placeholder="People", next=responsible, class="span11"}
                    ]},
            #panel{class="dropdown span3", body=[
                    #dropdown{id=responsible, value="", class="span12", data_fields=[
                            {provide, "typeahead"}, 
                            {source, Users}
                            ],
                              options=[
                            #option{text="Responsible", value="responsible"},
                            #option{text="Accountable", value="accountable"}
                            ]}

                    ]}
            ]}.
