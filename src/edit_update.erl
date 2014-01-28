%% -*- mode: nitrogen -*-
-module (edit_update).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-globe icon-2x'></i>".

buttons(left) ->
    "";
buttons(main) ->
    #panel{class='row-fluid', body=[
            #panel{class='span9 offset3', body=[
                    #panel{class="row-fluid", body=[
                            #button{ class='btn btn-link span2', body="<i class='icon-remove'></i> Discard", 
   					click=#script{script="window.history.back();"}},
                            #button{ class='btn btn-link span2', body="<i class='icon-ok'></i> Send", postback=save, delegate=?MODULE}
                            ]}
                    ]}
            ]}.

left() ->
    Subject = wf:session(current_subject),
    {ok, Updates} = db:get_updates_by_subject(Subject),
    [
    #panel{ class="span3", body=[
            #panel{id=files, class="row-fluid", body=[
                    common:render_files()
                            %"<i class='icon-th-large'></i> Select from my files", #br{}
                    ]},
            #panel{ class="row-fluid", body=[
                    case Updates of
                        [] ->
                            [];
                        U -> 
                            #panel{ class="span12", body=[
                                    "<i class='icon-file-alt'></i> Previous updates", #br{},
                                    [#update_preview{flag=false, age=Age, from=From, text=Text, icon="chevron-down"} || #db_update{from=From, date=Age, text=Text} <- U]
                                    ]}
                    end
                    ]}
                ]}].
body() ->
    #db_update{subject=Subject, to=To}  = wf:session(current_update),
    #panel{ class="span9", body=[
            #panel{ class="row-fluid", body=[
                    #panel{ class="input-prepend span11", body=[
                            #span{ class="add-on", body=[
                                    #span{html_encode=false, text="<i class='icon-globe'></i>"}
                                    ]},
                            #textbox{id=name, placeholder="Re:something", text=Subject, next=due, class="span12"}
                            ]}
                    ]},
            #addable_row{id=roles, num=0, body= #to{}},
            add_existing_rows(To),

            #panel{ class="row-fluid", body=[
                    #panel{class="span12", body=[
                            #textarea{class="input-block-level",rows=15, placeholder="Some text here", id=text}
                            ]}

                    ]}
%            #panel{ class="row-fluid", body=[
%                    #panel{class="span12", body=[
%                            #checkbox{id=notice,class="pull-left", text=" Send notice about this update to everyone involved",  checked=true}
%
%                            ]}
%
%                    ]}
            ]}.
            
add_existing_rows(To) when is_list(To) ->
    Tos = lists:zip(To, lists:seq(1, length(To))),
    lists:foreach(fun({ T, N }) ->
                {ok, #db_contact{id=CID, name=Name} } = db:get_contact_by_address(T),
                wf:session(wf:to_binary(Name), CID),
                element_addable_row:event({add, #addable_row{id=roles, num= N - 1, body=#to{text=Name}}})
        end, Tos),
    case length(Tos) of
        0 ->
            ok;
        _ ->
            element_addable_row:event({del, #addable_row{id=roles, num= 0}}),
            element_addable_row:event({add, #addable_row{id=roles, num= length(Tos), body=#to{}}})
    end,
    [];
add_existing_rows(_To) ->
    [].
    
event(save) ->
    Subject = wf:q(name),
    InvolvedS = wf:qs(person),
    Text = wf:q(text),
    Update = wf:session(current_update),
    #db_contact{id=UID} = wf:user(),
    Involved = lists:map(fun([]) ->
                    <<"">>;
                (N) ->
                    io:format("~p~n", [N]),
                    I = wf:session(wf:to_binary(N)),
                    io:format("~p~n", [I]),
                    {ok,  #db_contact{bitmessage=BM} } = db:get_contact(I),
                    BM
            end, InvolvedS) -- [<<"">>],
    io:format("~p~n", [Involved]),
    NUpdate = Update#db_update{subject=Subject, text=Text, from=UID, 
                               to=Involved,
                               date=date(), status=new},
    db:save(NUpdate),
    db:save_attachments(NUpdate, wf:session_default(attached_files, sets:new())),
    common:send_messages(NUpdate),
    wf:redirect("/");
event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).
