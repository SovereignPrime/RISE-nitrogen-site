%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_update_preview).
-include_lib("nitrogen_core/include/wf.hrl").
-include("protokol.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
         reflect/0,
         render_element/1
        ]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, update_preview).

-spec render_element(#update_preview{}) -> body().
render_element(#update_preview{icon=Icon, from=From, age=Age, subject=Subject, text=Data, flag=Flag, archive=Archive}) when Icon==3->
    {ok, #db_contact{name=FromName}} = db:get_contact_by_address(From),
    {Text, Attachments, Timestamp} = case Icon of
               3 ->
                   #message_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
                   {T, A, TS};
               4 ->
                   #task_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
                   {T, A, TS}
           end,
    TD = bm_types:timestamp() - Timestamp,
    CurrentSession = wf:session(current_subject),

    #panel{style="line-height:18px;margin-top:18px;", body=[
                                                            #panel{class="row-fluid no-padding", body=[
                                                                                                       #panel{class='span1 no-padding', body=["<i class='icon-globe'></i>"]},
                                                                                                       #panel{class='span7 no-padding', body=["<b>From: </b>", FromName]},
                                                                                                       #panel{class='span4 cell-right no-padding', body=[sugar:format_timedelta(TD)]}
                                                                                                      ]},
                                                            case Subject of 
                                                                undefined -> "";
                                                                Subject ->
                                                                    #panel{class='row-fluid', body=[
                                                                                                    #panel{class='span11 offset1 no-padding', style="overflow: hidden;", body=["<b>Subject: </b>", Subject]}
                                                                                                   ]}
                                                            end,
                                                            #panel{class='row-fluid', body=[
                                                                                            if Flag == true ->
                                                                                                   [
                                                                                                    if  CurrentSession == Subject ->
                                                                                                           #panel{class='span1', body=["<input type='checkbox' checked>"]};
                                                                                                       true ->
                                                                                                           #panel{class='span1', body=["<input type='checkbox'>"]}
                                                                                                    end,
                                                                                                    #panel{class='span11', body=[io_lib:format("~200s...", [Text])]}
                                                                                                   ];
                                                                                               true ->
                                                                                                   #panel{class='span12', body=[io_lib:format("~200s...", [Text])]}
                                                                                            end
                                                                                           ]}
                                                           ], actions=#event{type=click, postback={selected, Subject, Archive}}};
render_element(#update_preview{icon=Icon, from=From, age=Age, subject=Subject, text=Data, flag=Flag, archive=Archive}) when Icon==4 ->
    {ok, #db_contact{name=FromName}} = db:get_contact_by_address(From),
    #task_packet{text=Text, time=Timestamp} = binary_to_term(Data),
    TD = bm_types:timestamp() - Timestamp,
    CurrentSession = wf:session(current_subject),
    #panel{style="line-height:18px;margin-top:18px;", body=[
                                                            #panel{class="row-fluid no-padding", body=[
                                                                                                       #panel{class='span1 no-padding', body=["<i class='icon-calendar-empty'></i>"]},
                                                                                                       #panel{class='span7 no-padding', body=["<b>From: </b>", FromName]},
                                                                                                       #panel{class='span4 cell-right no-padding', body=[sugar:format_timedelta(TD)]}
                                                                                                      ]},
                                                            case Subject of 
                                                                undefined -> "";
                                                                Subject ->
                                                                    #panel{class='row-fluid', body=[
                                                                                                    #panel{class='span11 offset1 no-padding', style="overflow: hidden;", body=["<b>Subject: </b>", Subject]}
                                                                                                   ]}
                                                            end,
                                                            #panel{class='row-fluid', body=[
                                                                                            if Flag == true ->
                                                                                                   [
                                                                                                    if  CurrentSession == Subject ->
                                                                                                           #panel{class='span1', body=["<input type='checkbox' checked>"]};
                                                                                                       true ->
                                                                                                           #panel{class='span1', body=["<input type='checkbox'>"]}
                                                                                                    end,
                                                                                                    #panel{class='span11', body=[io_lib:format("~200s...", [Text])]}
                                                                                                   ];
                                                                                               true ->
                                                                                                   #panel{class='span12', body=[io_lib:format("~200s...", [Text])]}
                                                                                            end
                                                                                           ]}
                                                           ], actions=#event{type=click, postback={selected, Subject, Archive}}}.
