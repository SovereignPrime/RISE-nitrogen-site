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
render_element(#attachment{filename=File, size=Size, time=Time}) ->
    {Y, M, D} = Date,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    #panel{class="row-fluid", body=[
            #panel{class="span5", body=File},
            #panel{class="span1", body=Size},
            #panel{class="span4", body=Time},
            #panel{class="span2", body="<i class='icon-download-alt'></i>", style="text-align:center;"}
            ]}.

