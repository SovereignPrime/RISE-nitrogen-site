%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_file_row).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, file_row).

-spec render_element(#file_row{}) -> body().
render_element(_Record = #file_row{id=Id, name=Name, type=Type, size=Size, for=For, linked=Linked, date=Date, seed=Seed, peer=Peer, status=Status}) ->
    FType = case Type of
        "." ++ T ->
            string:to_upper(T);
        _ ->
            "BINARY"
    end,
                #tablerow{ cells=[
                        #tablecell{body=[
                                #checkbox{id=check,  postback={check, id}, checked=false}
                                ], class=""},
                        #tablecell{text=Name, class=""},
                        #tablecell{text=FType, class=""},
                        #tablecell{text=sugar:format_file_size(Size), class=""},
                        #tablecell{text=For, class=""},
                        #tablecell{text=Linked, class=""},
                        #tablecell{text=sugar:date_format(Date), class=""},
                        #tablecell{text=Seed, class=""},
                        #tablecell{text=Peer, class=""},
                        #tablecell{text=Status, class=""}
                        ]}.
