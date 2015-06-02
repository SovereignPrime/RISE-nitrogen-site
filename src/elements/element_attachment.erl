%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_attachment).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-include("torrent.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, attachment).

-spec render_element(#attachment{}) -> body().
render_element(#attachment{id=I,
                           fid=Id,
                           filename=File,
                           size=Size,
                           time=Time,
                           status=received} = Attachment) -> % {{{1
    {Y, M, D} = Time,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    PathId = wf:temp_id(),
    #panel{id=I, class="row-fluid", body=[
            #panel{class="span5", body=File},
            #panel{class="span1", body=sugar:format_file_size(Size)},
            #panel{class="span4", body=DateS},
            #panel{class="span2",
                   body=[
                         #hidden{id=PathId,
                                 actions=#event{type=changed, 
                                                postback={path, Id},
                                                delegate=?MODULE},
                         "<i class='icon-download-alt'></i>"
                        ],
                   style="text-align:center;",
                   actions=#event{type=click,
                                  postback={download, PathId, File},
                                  delegate=?MODULE}}
            ]};

render_element(Record=#attachment{id=I,
                                  fid=Id,
                                  filename=File,
                                  size=Size,
                                  time=Time,
                                  status=downloading}=Attachment) -> % {{{1
    {Y, M, D} = Time,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    {ok, Pid} = wf:comet(fun() ->
                                 receive 
                                     update ->
                                         wf:update(I, render_element(Attachment)),
                                         wf:flush()
                                 end
                         end),
    timer:send_after(10000, Pid, update),
    #panel{id=I, class="row-fluid", body=[
            #panel{class="span5", body=File},
            #panel{class="span1", body=wf:to_list(Size)},
            #panel{class="span4", body=DateS},
            #panel{class="span2", body=[
                            wf:to_list(0)
                    ], style="text-align:center;"}

            ]};

render_element(#attachment{id=I, fid=Id,
                           filename=File,
                           size=Size,
                           time=Time,
                           status=Status}) -> % {{{1
    {Y, M, D} = Time,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    #panel{id=I, class="row-fluid", body=[
            #panel{class="span6", body=File},
            #panel{class="span2", body=sugar:format_file_size(Size)},
            #panel{class="span3", body=DateS},
            #panel{class="span1", body="<i class='icon icon-save'></i>", style="text-align:center;", actions=#event{type=click, postback={save, File, Id}, delegate=?MODULE}}
            ]}.

event({path, FID, PathId}) -> % {{{1
    Path = wf:q(PathId),
    bitmessage:get_attachment(FID, Path);
event({download, Attachment, File}) -> % {{{1
    wf:redirect("/raw?id=" ++ Attachment ++ "&file=" ++ File).
