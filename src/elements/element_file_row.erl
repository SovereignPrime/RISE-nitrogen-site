%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_file_row).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-include("torrent.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, file_row).

-spec render_element(#file_row{}) -> body().
%% Rendrer filerow  {{{1
render_element(Record = #file_row{fid=FID,
                                  id=Id,
                                  name=Name,
                                  type=Type,
                                  size=Size,
                                  for=For,
                                  date=Date,
                                  status=Status}=File) ->
    FType = case Type of
        "." ++ T ->
            string:to_upper(T);
        _ ->
            "BINARY"
    end,
    {ok, Linked} = db:get_linked_messages(FID),
    {Stat, Percent, Uploaded} = {Status, 100, 0},
    % case ets:match_object(etorrent_torrent, #torrent{display_name=wf:to_binary(FID), _='_'}) of
                               %[#torrent{state=seeding, uploaded=U, all_time_uploaded=AU}] ->
                               %           if Status == downloading ->
                               %                  db:mark_downloaded(wf:to_list(FID)),
                               %                  {downloaded, 100, AU + U};
                               %              Status == uploaded ->
                               %                  {seeding, 100,AU + U};
                               %              true ->
                               %                  {Status, 100, AU + U}
                               %           end;
                               %[#torrent{ all_time_uploaded=AU, uploaded=U, left=Left, total=Total}] ->
                               %         {ok, Pid} = wf:comet(fun() ->
                               %                                      receive 
                               %                                          update ->
                               %                                              wf:replace(Id, render_element(File)),
                               %                                              wf:flush()
                               %                                      end
                               %                              end),
                               %         timer:send_after(10000, Pid, update),
                               %    {downloading, ((Total - Left)  * 100 div Total), AU + U};
                               %_ ->
                               %    {Status, 0, 0}
                           %end,
    Check =  sets:is_element(FID, wf:session_default(attached_files, sets:new())),

    #tablerow{id=Id, cells=[
            #tablecell{body=[
                            #checkbox{id=check,  postback={check, FID ,Check}, checked=Check}
                    ], class=""},
            #tablecell{text=Name, class=""},
            #tablecell{text=FType, class=""},
            #tablecell{text=sugar:format_file_size(Size), class=""},
            #tablecell{text=For, class=""},
            #tablecell{text=Linked, class=""},
            #tablecell{text=sugar:date_format(Date), class=""},
            case Stat of
                downloading ->
                    #tablecell{body=#progressbar{progress=wf:to_list(Percent),
                                                 width=80},
                               class=""};
                _ when Status == uploaded; Status == downloaded ->
                    [
                     #tablecell{text=Stat,
                                class=""},
                     #tablecell{body=
                                #panel{class="span1",
                                       body="<i class='icon icon-save'></i>",
                                       style="text-align:center;",
                                       actions=#event{type=click,
                                                      postback={save, Name, FID},
                                                      delegate=element_attachment} }}
                    ];
                received ->
                    [
                     #tablecell{text=Stat,
                                class=""},
                     #tablecell{body=
                                #panel{class="span2",
                                       body="<i class='icon-download-alt'></i>",
                                       style="text-align:center;",
                                       actions=#event{type=click,
                                                      postback={download, Record},
                                                      delegate=?MODULE}
                                      }}
                    ]
            end
            ]}.

event({download, #file_row{id=I, fid=Id} = Attachment}) -> % {{{1
    {ok, [ File ]} = db:get_files([Id]),
    common:get_torrent(Id), 
    db:save(File#db_file{status=downloading}),
    wf:replace(I, Attachment#file_row{status=downloading});
event(E) ->
    error_logger:warning_msg("Wrong event ~p in ~p", [E, ?MODULE]).
