%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_expander).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields,expander).

-spec render_element(#expander{}) -> body().
render_element(_Record = #expander{start=Start, data_fields=Data, class=Class, target=Target }) ->
    Id = wf:temp_id(),  
    #span{id=Id, data_fields=Data, class=[expander, Class,open_or_closed(Start)], style="width:10px; display:inline-block", actions=#event{type=click, actions=[
        wf:f("var me = objs('~s');",[Id]),
        "if(me.hasClass('icon-caret-down')) {",
            "me.removeClass('icon-caret-down').addClass('icon-caret-right');",
            wf:f("objs('~s').hide();",[Target]),
        "}else{",
            "me.removeClass('icon-caret-right').addClass('icon-caret-down');",
            wf:f("objs('~s').show();",[Target]),
        "}"
    ]}}.
    


open_or_closed(open) ->
    'icon-caret-down';
open_or_closed(closed) ->
    'icon-caret-right'.
    
