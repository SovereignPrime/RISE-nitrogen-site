%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_file_row).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("torrent.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, file_row).

-spec render_element(#file_row{}) -> body().
render_element(_Record = #file_row{fid=FID, id=Id, name=Name, type=Type, size=Size, for=For, date=Date, status=Status}) ->
    FType = case Type of
        "." ++ T ->
            string:to_upper(T);
        _ ->
            "BINARY"
    end,
    {ok, Linked} = db:get_linked_messages(FID),
    {Leechers, Seed, Percent} = case ets:match_object(etorrent_torrent,#torrent{display_name=wf:to_binary(Id), _='_'}) of
        [#torrent{leechers=L, seeders=S, left=Left, total=Total}] ->
            {L, S, ((Total - Left)  * 100 / Total)};
        [] ->
            {0, 0, 0}
    end,
    Peer = Leechers + Seed,
    Check =  sets:is_element(FID, wf:session_default(attached_files, sets:new())),
    #tablerow{ cells=[
            #tablecell{body=[
                            #checkbox{id=check,  postback={check, FID ,Check}, checked=Check}
                    ], class=""},
            #tablecell{text=Name, class=""},
            #tablecell{text=FType, class=""},
            #tablecell{text=sugar:format_file_size(Size), class=""},
            #tablecell{text=For, class=""},
            #tablecell{text=Linked, class=""},
            #tablecell{text=sugar:date_format(Date), class=""},
            %#tablecell{text=Seed, class=""},
            %#tablecell{text=Peer, class=""},
            case Status of
                downloading ->
                    #tablecell{body=#progressbar{progress=Percent, width=80}, class=""};
                _ ->
                    #tablecell{text=Status, class=""}
            end
            ]}.
