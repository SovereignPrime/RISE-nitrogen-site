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
    #panel{class='row-fluid', body=[

    #panel{class='span9 offset2', body=[
            #panel{class="row-fluid", body=[
                    #panel{ class='span2', body="<i class='icon-reorder'></i> More options"},
                    #panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                    #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                    #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                    #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                    ]}
            ]}]}.

left() ->
    [].

body() -> 
    {ok, Files} = db:all_files(),
    [
        #table{ rows=[
                #tablerow{ cells=[
                        #tablecell{body=[
                                #checkbox{id=check_all,  postback=check_all, checked=false, delegate=common}
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

event(Click) ->
    io:format("~p~n",[Click]).
