%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-globe icon-2x'></i>".

buttons() ->
    {ok, New} = db:get_unread_updates(),
    #panel{class='row-fluid', body=[
            #panel{class='span1 offset1', body=[
                    #span{id=count, class='label label-inverse',text=wf:f("~p new", [length(New)])}
                    ]},
            #panel{class='span9 offset1', body=[
                    #panel{class="row-fluid", body=[
                            #panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                            #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                            #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                            #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                            ]}
                    ]}]}.

left() ->
    {ok, Updates} = db:get_updates(),
    #panel{id=left,class="span3", body=[ #update_preview{icon="globe", from=From, age=Age, subject=Subject, text=Text, flag=true} || 
            #db_update{from=From, date=Age, subject=Subject, text=Text} <- Updates]}.

body() ->
    #panel{id=body, class="span9", body=render_body("test")}.

render_body(Subject) ->
    {ok, Updates} = db:get_updates_by_subject(Subject),
    [
        #h1{html_encode=false, text="<i class='icon-globe'></i> " ++ Subject},
        [
        #update_element{collapse=true, from=From, text=Text, age= Age, uid=Id, subject=Subject} || #db_update{id=Id, subject=Subject, from=From, text=Text, date=Age} <- Updates
            ]


            ].
	
    
event({selected, Subject}) ->
    wf:update(body, render_body(Subject));
event({unfold, #update_element{id=Id, uid=Uid}=Update}) ->
    {ok,Attachments} = db:get_attachments(#db_update{id=Uid}),
    wf:replace(Id, Update#update_element{collapse=false, attachments=[
                #attachment{filename=File, size=Size, time=Time} || #db_file{path=File, size=Size, date=Time} <- Attachments
                                                               ]});
event({fold, #update_element{id=Id}=Update}) ->
    wf:replace(Id, Update#update_element{collapse=true});
event({reply, Subject}) ->
    {ok, Id} = db:next_id(db_update),
    wf:session(current_subject, Subject),
    wf:session(current_update, #db_update{id=Id, subject=Subject}),
    wf:session(attached_files, undefined),
    wf:redirect("/edit_update");
event(Click) ->
    io:format("Event ~p in ~p~n", [Click, ?MODULE]).
