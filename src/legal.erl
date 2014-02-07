%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (legal).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> 
    PWD = application:get_env(nitrogen, work_dir, "."),
    {ok, Pid} = wf:comet(fun() -> counter(1) end),
    %spawn_link(db, install, [Pid]),
    timer:send_interval(1000, Pid, timeout),
    #template { file= PWD ++ "/site/templates/legal.html" }.
	
accept() ->
    #link{id=accept,class="btn btn-link", body=[
                "<i class='icon-ok'></i> Accept &nbsp;"
               ], postback=accept, new=false}.
    
decline() ->
    #link{class="btn btn-link", body="<i class='icon-remove'></i> Decline", postback=decline, new=false}.

progress() ->
    #panel{id=progress,  style="width:600px;height:3px;display:inline-block;", body=[
                                                                          #panel{text=" ", style="width:3px;height:3px;background-color:#000;display:inline-block;"}
                                                                         ]}.

event(accept) ->
    ok;
event(decline) ->
    wf:wire(#alert{text="If you don't agree just close and remove RISE from your computer, thanks"});
event(click) ->
    wf:insert_top(placeholder, "<p>You clicked the button!").

counter(N) ->
    receive
        timeout ->
            if N > 200 ->
                   wf:remove(pr),
                   wf:flush(),
                   counter(0);
               true ->
                   wf:insert_bottom(progress, #panel{id=pr, text=" ", style="width:3px;background-color:#000;height:3px;display:inline-block;"}),
                   wf:flush(),
                   counter(N+1)
            end;
        accept ->
            wf:replace(pl, #panel{style="text-align:center;", body=[
                                                                          #panel{ style="display:inline-block;width:170px;", body=[

                                                                                                                                   #link{id=accept,class="btn btn-link", body=[
                                                                                                                                                                               "<i class='icon-ok'></i> Accept &nbsp;"
                                                                                                                                                                              ], url="/", new=false}
                                                                                                                                  ]},
                                                                          #panel{ style="display:inline-block;width:170px;", body=[
                                                                                                                                   #link{class="btn btn-link", body="<i class='icon-remove'></i> Decline", postback=decline, new=false}
]}
]})
    end.

