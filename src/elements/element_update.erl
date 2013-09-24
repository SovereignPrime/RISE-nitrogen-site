%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_update).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, update_element).

-spec render_element(#update_element{}) -> body().
render_element(#update_element{from=From, text=Text, age=Age, collapse=true}) ->
        #panel{class="row-fluid", body=[
                #panel{class="span2", body="<i class='icon-chevron-down'></i> " ++ From},
            #panel{class="span8", body=io_lib:format("~100s", [Text])},
                #panel{class="span2", body=Age}
                ]};
render_element(#update_element{from=From, text=Text, age=Age, collapse=false}) ->
    [
        #panel{class="row-fluid", body=[
                #panel{class="span1", body=From},
                #panel{class="span2 offset9", body=Age}
                ]},
        #panel{class="row-fluid", body=[
                #panel{class="span12", body=Text}
                ]}
        ].

