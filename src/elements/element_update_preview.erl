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
render_element(#update_preview{icon=Icon, from=From, age=Age, subject=Subject, text=Text}) ->
                io_lib:format("<div class='row-fluid'>
                    <div class='span1'><i class='icon-~s'></i></div>
                    <div class='span8'><b>From: </b>~s</div>
                    <div class='span3'>~s</div>
                </div>
                <div class='row-fluid'>
                    <div class='span11 offset1'><b>Subject: </b>~s</div>
                </div>
                <div class='row-fluid'>
                    <div class='span1'><input type='checkbox'></div>
                    <div class='span11'>~200s...</div>
                              </div>", [Icon, From, Age, Subject, Text]).
