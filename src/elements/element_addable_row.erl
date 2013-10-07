%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_addable_row).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, addable_row).

-spec render_element(#addable_row{}) -> body().
render_element(Record = #addable_row{id=Id, num=N, body=Body}) ->
    #panel{id= wf:to_atom(wf:f("~s_addable~p", [Id, N])), class="row-fluid", body=[
            #panel{ class="input-prepend span11", body=[Body]},
            #panel{class="span1", body=[
                    #button{id=wf:to_atom(wf:f("~s_addable_plus~p", [Id, N])), 
                            body=["<i class='icon-plus'></i>"], html_encode=false, postback={add,  Record}, delegate=?MODULE}
                    ]}

            ]}.

event({add, #addable_row{id=Id, num=N, body=Body, options=undefined}=Record}) ->
    io:format("Event ~s in module ~p~n", [Id, ?MODULE]),
    wf:replace(wf:f("~s_addable_plus~p",[Id, N]), 
                    #button{id=wf:to_atom(wf:f("~s_addable_plus~p", [Id, N])), 
                            body=["<i class='icon-minus'></i>"], html_encode=false, postback={del,  Record}, delegate=?MODULE}
             ),
    wf:insert_after(wf:f("~s_addable~p", [Id, N]), 
                    #addable_row{
            id=Id,
            num=N+1,
            body=Body
            });
event({add, #addable_row{id=Id, num=N, body=Body, options=Opt}=Record}) ->
    io:format("Event ~p in module ~p~n", [Record, ?MODULE]),
    wf:replace(wf:f("~s_addable_plus~p",[Id, N]), 
               #panel{class="dropdown span12", body=[
                "<a href='#', class='btn dropdown-toggle' data-toggle='dropdown'>",
                "<i class='icon-reorder'></i>",
                "</a>",
                #list{numbered=false, class="dropdown-menu",
                      body=[
                        #listitem{body=[
                                #link{id=wf:to_atom(wf:f("~s_addable_plus~p", [Id, N])), 
                                        body=["<i class='icon-minus'></i> Delete"], html_encode=false, postback={del,  Record}, delegate=?MODULE}
                                ]},
                        Opt(Id, N)
                        ]}
                ]}
             ),
    wf:insert_after(wf:f("~s_addable~p", [Id, N]), 
                    #addable_row{
            id=Id,
            num=N+1,
            options=Opt,
            body=Body
            });
event({del, #addable_row{id=Id, num=N}=_Record}) ->
    wf:remove(wf:f("~s_addable~p",[Id, N]));
event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).
