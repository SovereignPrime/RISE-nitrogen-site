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
render_element(#attachment{id=I, fid=Id, filename=File, size=Size, time=Time, status=received} = Attachment) ->
    {Y, M, D} = Time,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    #panel{id=I, class="row-fluid", body=[
            #panel{class="span5", body=File},
            #panel{class="span1", body=sugar:format_file_size(Size)},
            #panel{class="span4", body=DateS},
            #panel{class="span2", body="<i class='icon-download-alt'></i>", style="text-align:center;", actions=#event{type=click, postback={download, Attachment}, delegate=?MODULE}}
            ]};
render_element(#attachment{id=I, fid=Id, filename=File, size=Size, time=Time, status=Status}) when Status==uploaded; Status==downloaded ->
    {Y, M, D} = Time,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    #panel{id=I, class="row-fluid", body=[
            #panel{class="span6", body=File},
            #panel{class="span2", body=sugar:format_file_size(Size)},
            #panel{class="span3", body=DateS},
            #panel{class="span1", body="<i class='icon icon-save'></i>", style="text-align:center;", actions=#event{type=click, postback={save, File, Id}, delegate=?MODULE}}
            ]};
render_element(Record=#attachment{id=I, fid=Id, filename=File, size=Size, time=Time, status=downloading}) ->
    {Y, M, D} = Time,
    DateS = io_lib:format("~p-~p-~p", [Y, M, D]),
    #panel{id=I, class="row-fluid", body=[
            #panel{class="span5", body=File},
            #panel{class="span1", body=wf:to_list(Size)},
            #panel{class="span4", body=DateS},
            #panel{class="span2", body=[
                    case ets:match_object(etorrent_torrent,#torrent{display_name=wf:to_binary(Id), _='_'}) of
                        [#torrent{state=seeding}] ->
                            db:mark_downloaded(wf:to_list(Id)),
                            render_element(Record#attachment{status=downloaded});
                        [#torrent{leechers=L, seeders=S, left=Left, total=Total}] ->
                            wf:to_list((Total - Left)  * 100 / Total);
                        [] ->
                            wf:to_list(0)
                    end
                    ], style="text-align:center;"}

            ]}.

event({save, File, Id}) ->
    wf:redirect("/raw?id=" ++ Id ++ "&file=" ++ File);
event({download, #attachment{id=I, fid=Id} = Attachment}) ->
    {ok, [ File ]} = db:get_files([Id]),
    common:get_torrent(Id), 
    db:save(File#db_file{status=downloading}),
    io:format("~p~n", [I]),
    wf:replace(I, Attachment#attachment{status=downloading}).
