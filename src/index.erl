%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
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

buttons(main) -> % {{{1
    #list{class="nav nav-pills", style="display:inline-block;", numbered=false,
          body=[
                #listitem{body=[
                                common:render_filters()
                               ]},
                #listitem{body=[
                                #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}
                               ]},
                #listitem{body=[
                                common:settings_menu()
                               ]}
               ]}.

left() -> % {{{1
    left(false).

left(Archive) -> % {{{1
    case db:get_updates(Archive) of 
        {ok, none} ->
            render_left([]);
        {ok, Updates} ->
            render_left(Updates)
    end.

render_left(Updates) -> % {{{1
    Render = [ #update_preview{id=Id,
                               icon=Enc,
                               from=From,
                               to=To,
                               age="Age",
                               subject=Subject,
                               text=Text,
                               flag=true,
                               archive=(Status == archive)} || 
               #message{hash=Id,
                        from=From,
                        to=To,
                        subject=Subject,
                        text=Text,
                        enc=Enc,
                        status=Status} <- sugar:sort_by_timestamp(Updates)],
    #panel{id=left,class="span3 scrollable", body=Render}.

body() -> % {{{1
    body(false).

body(Archive) -> % {{{1
    #panel{id=body,
           class="span9 scrollable",
           body=case wf:session(current_subject) of
                    undefined ->
                        [];
                    Subject ->
                        render_body(Subject, Archive)
                end
          }.

render_body(Subject, Archive) -> % {{{1
    wf:session(current_subject, Subject),
    {ok, Updates} = db:get_updates_by_subject(Subject, Archive),
    [
     #h1{html_encode=false, text="<i class='icon-message'></i> " ++ Subject},
     [
      #update_element{collapse=true,
                      from=From,
                      to=To,
                      text=Text,
                      uid=Id,
                      subject=Subject,
                      enc=Enc,
                      status=Status} || #message{hash=Id,
                                                 enc=Enc,
                                                 to=To,
                                                 subject=Subject,
                                                 from=From,
                                                 text=Text,
                                                 status=Status} <- sugar:sort_by_timestamp(Updates)
     ] 
    ].

event({selected, Id, Subject, Archive}) -> % {{{1
    wf:session(current_subject, Subject),
    wf:session(current_update_id, Id),
    wf:replace(left, left(Archive)),
    wf:update(body, render_body(Subject, Archive)),
    wf:wire("$(\".update-preview\").has(\"input[type=checkbox]:checked\").addClass(\"related-message\");"),
    wf:wire("$(\".update-preview\").has(\"input[type=checkbox]:not(:checked)\").removeClass(\"related-message\");");

event({archive, E, Rec}) -> % {{{1
    {ok, #message{subject=Subject}} = db:archive(#message{hash=Rec}),
    wf:replace(left, left()),
    wf:update(body, render_body(Subject, false));

event({show_archive, true}) -> % {{{1
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    {ok, Updates} = db:get_updates(true),
    #db_contact{id=My} = wf:user(),
    {ok, Tasks} = db:get_tasks(true),
    wf:replace(left, render_left(Updates)),
    wf:replace(body, body(true));

event({show_archive, false}) -> % {{{1
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    wf:replace(left, left()),
    wf:replace(body, body());

event(Click) -> % {{{1
    io:format("Event ~p in ~p~n", [Click, ?MODULE]).

incoming() -> % {{{1
    wf:replace(left, left()).
