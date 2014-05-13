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

buttons(left) ->
    "";
buttons(main) ->
    #list{numbered=false, class="nav nav-pills", style="display:inline-block;",body=[
        case wf:q(from) of
            "task" -> done_button("/edit_task","Add to Task");
            "message" -> done_button("/edit_update", "Add to Message");
            _ -> more_options_buttons()
        end,
        #listitem{body=[
            %#panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
        ]},
        #listitem{body=[
            common:render_filters()
        ]},
        #listitem{body=[
            %#panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
        ]},
        #listitem{body=[
            #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}
        ]},
        #listitem{body=[
            common:settings_menu()
        ]},
        #listitem{body=[
            common:render_help()
        ]}
    ]}.

done_button(Url, Label) ->
    #listitem{body=[
        #link{class="btn btn-link", url=Url, new=false, body=[
            "<i class='icon-ok'></i>Done, ", Label
        ]}
    ]}.

more_options_buttons() ->
    #listitem{class="dropdown", body=[
        #link{class="btn btn-link dropdown-toggle",data_fields=[{toggle, "dropdown"}], url="#", new=false, body=[
            "<i class='icon-reorder'></i> More options"
        ]},
        #list{class="dropdown-menu", numbered=false, body=[
            #listitem{body=#link{body=["<i class='icon icon-list-alt'></i> Archive selected"], postback=archive, new=false}},
            #listitem{body=#link{body="Add to message", url = "/edit_update", new=false}},
            #listitem{body=#link{body="Add to task", url = "/edit_task", new=false}}%,
            %#listitem{body=#link{body="Add to expense", postback=edit, new=false}},
            %#listitem{body=#link{body="Add to assert", postback=edit, new=false}}
        ]}
    ]}.


left() ->
    [].

body() ->
    body(false).
body(Archive) -> 
    {ok, Files} = db:get_files(Archive),
    #panel{id=body, body=[
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
                        #tableheader{text="Uploaded size", class=""},
                        %#tableheader{text="Peer", class=""},
                        #tableheader{text="Status", class=""}
                        ]},
                lists:map(fun(#db_file{id=Id, path=Name, size=Size, type=Type,user=For, date=Date, status=Status} ) ->
                            {ok, #db_contact{name=U}} = db:get_contact(For),
                            #file_row{fid=Id, name=Name, size=Size, type=Type, for=U, date=Date, status=Status}
                    end, Files)
                ]}

            ]}.    

event(archive) ->
    Files = sets:to_list(wf:session_default(attached_files, sets:new())),
    db:archive(Files),
    wf:replace(body, body(false));
event({show_archive, true}) ->
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    wf:replace(body, body(true));
event({show_archive, false}) ->
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    wf:replace(body, body(false));
event({check, FID, true}) ->
    AF = wf:session_default(attached_files, sets:new()),
    wf:session(attached_files,  sets:del_element( FID, AF));
event({check, FID, false}) ->
    AF = wf:session_default(attached_files, sets:new()),
    wf:session(attached_files,  sets:add_element( FID, AF));
event(Click) ->
    io:format("~p~n",[Click]).

incoming() ->
    wf:replace(body, body()).
