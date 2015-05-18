%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_rise_upload).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, rise_upload).

-spec render_element(#rise_upload{}) -> body().
render_element(_Record = #rise_upload{
                           id=Id,
                           class=Class,
                           tag=Tag,
                           droppable_text=Text
                           }) ->
    PathId = wf:to_atom("upload_path_" ++ wf:to_list(Tag)),
    wf:wire(#script{script= "$.getScript('/js/upload.js', function(data, status, xnr) {" ++
                            "console.log(data);" ++
                            "init_upload('" ++ PathId ++ "');" ++ 
                            "});"}),
    [#panel{id=Id,
           class=["rise_upload" | Class],
           text=wf:html_encode(Text),
          },
     #hiden{id=PathId}
    ].
