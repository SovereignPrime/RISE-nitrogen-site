%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (files).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> "<i class='icon-file-text-alt icon-2x'></i>".

buttons() ->
    #panel{class="row-fluid", body=[
            #panel{ class='span2 offset2 btn-group', body=[
                    #link{class="btn btn-link dropdown-toggle",data_fields=[{toggle, "dropdown"}], 
                          body=["<i class='icon-reorder'></i> More options"], url="#", new=false},
                    #list{class="dropdown-menu", numbered=false,
                          body=[
                            #listitem{body=#link{body=["<i class='icon icon-list-alt'></i> Archive selected"], postback=archive, new=false}},
                            #listitem{body=#link{body="Add to message", url = "/edit_update", new=false}},
                            #listitem{body=#link{body="Add to task", url = "/edit_task", new=false}}%,
                            %#listitem{body=#link{body="Add to expense", postback=edit, new=false}},
                            %#listitem{body=#link{body="Add to assert", postback=edit, new=false}}
                            ]}

                    ]},
            %#panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
            #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
            #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
            #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
            ]}.

left() ->
    [].

body() -> 
    {ok, Files} = db:all_files(),
    [
        #table{ rows=[
                #tablerow{ cells=[
                        #tablecell{body=[
                                %#checkbox{id=check_all,  postback=check_all, checked=false, delegate=common}
                                ], class=""},
                        #tableheader{text="File name", class=""},
                        #tableheader{text="Type", class=""},
                        #tableheader{text="Size", class=""},
                        #tableheader{text="From/To", class=""},
                        #tableheader{text="Linked message", class=""},
                        #tableheader{text="Date", class=""},
                        #tableheader{text="Seed", class=""},
                        #tableheader{text="Peer", class=""},
                        #tableheader{text="Status", class=""}
                        ]},
                lists:map(fun(#db_file{id=Id, path=Name, size=Size, type=Type,user=For, date=Date, status=Status} ) ->
                            {ok, #db_contact{name=U}} = db:get_contact(For),
                            #file_row{fid=Id, name=Name, size=Size, type=Type, for=U, date=Date, status=Status}
                    end, Files)
                ]}

        ].    

event({check, FID, true}) ->
    AF = wf:session_default(attached_files, sets:new()),
    wf:session(attached_files,  sets:del_element( FID, AF));
event({check, FID, false}) ->
    AF = wf:session_default(attached_files, sets:new()),
    wf:session(attached_files,  sets:add_element( FID, AF));
event(Click) ->
    io:format("~p~n",[Click]).
