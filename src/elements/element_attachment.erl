%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_attachment).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, attachment).

-spec render_element(#attachment{}) -> body().
render_element(#attachment{filename=File, size=Size, time=Time, status=received}) ->
    {Y, M, D} = Time,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    #panel{class="row-fluid", body=[
            #panel{class="span5", body=File},
            #panel{class="span1", body=wf:to_list(Size)},
            #panel{class="span4", body=DateS},
            #panel{class="span2", body="<i class='icon-download-alt'></i>", style="text-align:center;", actions=#event{type=click, postback={download, File}, delegate=?MODULE}}
            ]};
render_element(#attachment{filename=File, size=Size, time=Time, status=Status}) when Status==uploaded; Status==downloaded ->
    {Y, M, D} = Time,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    #panel{class="row-fluid", body=[
            #panel{class="span6", body=File},
            #panel{class="span2", body=wf:to_list(Size)},
            #panel{class="span3", body=DateS},
            #panel{class="span1", body="<i class='icon icon-save'></i>", style="text-align:center;", actions=#event{type=click, postback={save, File}, delegate=?MODULE}}
            ]};
render_element(#attachment{filename=File, size=Size, time=Time, status=downloadeing}) ->
    {Y, M, D} = Time,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    #panel{class="row-fluid", body=[
            #panel{class="span5", body=File},
            #panel{class="span1", body=wf:to_list(Size)},
            #panel{class="span4", body=DateS},
            #panel{class="span2", body=[
                    #range{id=progress, value=10}
                    ], style="text-align:center;"}
            ]}.
