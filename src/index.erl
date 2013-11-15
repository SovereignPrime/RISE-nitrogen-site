%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-globe icon-2x'></i>".

buttons() ->
    {ok, New} = db:get_unread_updates(),
    wf:session(unread, length(New)),
    #panel{id=buttons, class='row-fluid', body=[
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
    #panel{id=left,class="span3 scrollable", body=[ #update_preview{icon="globe", from=From, age=Age, subject=Subject, text=Text, flag=true} || 
            #db_update{from=From, date=Age, subject=Subject, text=Text} <- lists:reverse(Updates)]}.

body() ->
    case db:get_updates() of
        {ok, []} ->
            [];
        {ok, [ #db_update{subject=Subject} | _Updates ]} ->
            #panel{id=body, class="span9 scrollable", body=render_body(Subject)}
    end.

render_body(Subject) ->
    {ok, Updates} = db:get_updates_by_subject(Subject),
    [
        #h1{html_encode=false, text="<i class='icon-globe'></i> " ++ Subject},
        [
        #update_element{collapse=true, from=From, to=To, text=Text, age= Age, uid=Id, subject=Subject} || #db_update{id=Id, to=To, subject=Subject, from=From, text=Text, date=Age} <- lists:reverse(Updates)
            ]


            ].
	
    
event({selected, Subject}) ->
    wf:update(body, render_body(Subject));
event({unfold, #update_element{id=Id, uid=Uid}=Update}) ->
    {ok,Attachments} = db:get_attachments(#db_update{id=Uid}),
    case db:set_read(Uid) of
        {ok, unread} ->
                New = wf:session(unread) - 1,
                wf:session(unread, New),
                wf:replace(count, #span{id=count, class='label label-inverse',text=wf:f("~p new", [New])});
        {ok, _} ->
            ok
    end,

    wf:replace(Id, Update#update_element{collapse=false, attachments=[
                #attachment{fid=FId, filename=File, size=Size, time=Time, status=Status} || #db_file{id=FId, path=File, size=Size, date=Time, status=Status} <- Attachments
                                                               ]});
event({fold, #update_element{id=Id}=Update}) ->
    wf:replace(Id, Update#update_element{collapse=true});
event({reply, Subject, To}) ->
    {ok, Id} = db:next_id(db_update),
    wf:session(current_subject, Subject),
    wf:session(current_update, #db_update{id=Id, to=To, subject=Subject}),
    wf:session(attached_files, undefined),
    wf:redirect("/edit_update");
event(Click) ->
    io:format("Event ~p in ~p~n", [Click, ?MODULE]).

incoming() ->
    wf:replace(left, left()),
    {ok, New} = db:get_unread_updates(),
    wf:session(unread, length(New)),
    wf:replace(count, #span{id=count, class='label label-inverse',text=wf:f("~p new", [length(New)])}),
    wf:flush().
