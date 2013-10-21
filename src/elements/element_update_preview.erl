%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_update_preview).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, update_preview).

-spec render_element(#update_preview{}) -> body().
render_element(#update_preview{icon=Icon, from=From, age=Age, subject=Subject, text=Text, flag=Flag}) ->
    #panel{body=[
        #panel{class="row-fluid", body=[
                #panel{class='span1', body=["<i class='icon-" ++ Icon ++ "'></i>"]},
                #panel{class='span7 ', body=["<b>From: </b>", From]},
                #panel{class='span4 cell-right', body=[sugar:date_format(Age)]}
                ]},
        case Subject of 
            undefined -> "";
            Subject ->
                #panel{class='row-fluid', body=[
                        #panel{class='span11 offset1', body=["<b>Subject: </b>", Subject]}
                        ]}
        end,
        #panel{class='row-fluid', body=[
                if Flag == true ->
                        #panel{class='span1', body=["<input type='checkbox'>"]},
                        #panel{class='span11', body=[io_lib:format("~200s...", [Text])]};
                    true ->
                        #panel{class='span12', body=[io_lib:format("~200s...", [Text])]}
                end
                ]}
            ], actions=#event{type=click, postback={selected, Subject}}}.
