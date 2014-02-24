%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").
-include("protokol.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-globe icon-2x' style='margin-top:-5px;'></i>".

buttons(left) ->
    {ok, New} = db:get_unread_updates(),
    wf:session(unread, length(New)),
    #span{id=count, class='label label-inverse',text=wf:f("~p new", [length(New)])};
buttons(main) ->
    #list{class="nav nav-pills", style="display:inline-block;", numbered=false,
          body=[
%                #listitem{body=[
%                                %#panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
%                               ]},
%                #listitem{body=[
%                                %#panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
%                               ]},
                #listitem{body=[
                                common:render_filters()
                               ]},
%                #listitem{body=[
%                                %#panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
%                               ]},
                #listitem{body=[
                                #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}
                               ]},
                #listitem{body=[
                                common:settings_menu()
                               ]}
               ]}.

left() ->
    left(false).
left(Archive) ->
    case db:get_updates(Archive) of
        {ok, none} ->
            render_left([]);
        {ok, Updates} ->
            render_left(Updates)
    end.
render_left(Updates) ->
    Render = [ #update_preview{id=Id, icon=Enc, from=From, age="Age", subject=Subject, text=Text, flag=true, archive=(Status == archive)} || 
      #message{hash=Id, from=From, subject=Subject, text=Text, enc=Enc, status=Status} <- sugar:sort_by_timestamp(Updates)],
    #panel{id=left,class="span3 scrollable", body=Render}.

body() ->
    body(false).

body(Archive) ->
    case db:get_updates(Archive) of
        {ok, []} ->
            [];
        {ok, [ #message{subject=Subject} | _Updates ]} ->
            #panel{id=body, class="span9 scrollable", body=render_body(wf:session_default(current_subject, Subject), Archive)}
    end.

render_body(Subject, Archive) ->
    wf:session(current_subject, Subject),
    {ok, Updates} = db:get_updates_by_subject(Subject, Archive),
    [
        #h1{html_encode=false, text="<i class='icon-globe'></i> " ++ Subject},
        [
        #update_element{collapse=true, from=From, to=To, text=Text, uid=Id, subject=Subject, enc=Enc} || #message{hash=Id, enc=Enc, to=To, subject=Subject, from=From, text=Text} <- sugar:sort_by_timestamp(Updates)
            ] 
            ].
	
    
event({selected, Id, Subject, Archive}) ->
    wf:session(current_subject, Subject),
    wf:session(current_update_id, Id),
    wf:replace(left, left(Archive)),
    wf:update(body, render_body(Subject, Archive));
event({unfold, #update_element{id=Id, uid=Uid, enc=Enc}=Update}) ->
    case Enc of
        3 ->
            {ok,Attachments} = db:get_attachments(#db_update{id=Uid});
        4 ->
            {ok,Attachments} = db:get_attachments(#db_task{id=Uid})
    end,

    case db:set_read(Uid) of
        {ok, new} ->
                New = wf:session(unread) - 1,
                wf:session(unread, New),
                wf:replace(count, #span{id=count, class='label label-inverse',text=wf:f("~p new", [New])});
        {ok, _} ->
            ok
    end,

    wf:replace(Id, Update#update_element{collapse=false, attachments=[
                #attachment{fid=FId, filename=File, size=Size, time=Time, status=Status} || #db_file{id=FId, path=File, size=Size, date=Time, status=Status} <- Attachments
                                                               ]});
event({archive, E, Rec}) ->
    {ok, #message{subject=Subject}} = db:archive(#message{hash=Rec}),
    wf:replace(left, left()),
    wf:update(body, render_body(Subject, false));
event({show_archive, true}) ->
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    {ok, Updates} = db:get_updates(true),
    #db_contact{id=My} = wf:user(),
    {ok, Tasks} = db:get_tasks(true),
    wf:replace(left, render_left(Updates)),
    wf:replace(body, body(true));
event({show_archive, false}) ->
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    wf:replace(left, left()),
    wf:replace(body, body());
event({fold, #update_element{id=Id}=Update}) ->
    wf:replace(Id, Update#update_element{collapse=true});
event({reply, Subject, To}) ->
    {ok, Id} = db:next_id(db_update),
    wf:session(current_subject, Subject),
    wf:session(current_update, #db_update{id=Id, to=[ To ], subject=Subject}),
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
