%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_update).
-include_lib("nitrogen_core/include/wf.hrl").
-include("protokol.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, update_element).

-spec render_element(#update_element{}) -> body().
render_element(#update_element{id=Id, from=From, text=Data, age=Age, collapse=true, enc=Enc, status=Status}=Record) -> % {{{1
    FromName = case db:get_contact_by_address(From) of
                   {ok, #db_contact{name=FN}} ->
                       FN;
                   {ok, none} ->
                       wf:to_list(From)
               end,
    {Text, Attachments, Timestamp} = case Enc of
                                         3 ->
                                             #message_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
                                             {T, A, TS};
                                         4 ->
                                             #task_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
                                             {T, A, TS};
                                         5 ->
                                             #update_packet{text=T, time=TS} = binary_to_term(Data),
                                             {T, [], TS}
           end,
    TD = bm_types:timestamp() - Timestamp,
        #panel{id=Id, class="row-fluid", body=[
                #panel{class="span2", body="<i class='icon-chevron-down'></i> " ++ FromName},
                #panel{class="span8", body=io_lib:format("~100s", [Text])},
                #panel{class="span2", body=[
                                            sugar:format_timedelta(TD),
                                            case Status of
                                                S when S==read; S==sent ->
                                                    #image{image="/img/read.svg",  style="height: 16px;vertical-align:middle;margin-left: 5px;"};
                                                S when S==unread; S==ackwait ->
                                                    #image{image="/img/unread.svg",  style="height: 16px;vertical-align:middle;margin-left: 5px;"};
                                                S when S==new; S==wait_pubkey; S==encrypting ->
                                                    "&nbsp;<i class='icon-mail-forward'></i>"
                                            end
                                           ]}
            ], actions=#event{type=click, postback={unfold, Record}, delegate=common}};
render_element(#update_element{id=Id, uid=UID, from=From, to=To, text=Data, age=Age, subject=Subject, collapse=false, enc=Enc, status=Status}=Record) -> % {{{1
    FromName = case db:get_contact_by_address(From) of
                   {ok, #db_contact{name=FN}} ->
                       FN;
                   {ok, none} ->
                       wf:to_list(From)
               end,
    {Text, Attachments, Timestamp} = case Enc of
                                         3 ->
                                             #message_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
                                             {T, A, TS};
                                         4 ->
                                             #task_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
                                             {T, A, TS};
                                         5 ->
                                             #update_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
                                             TB = [T, #panel{id=command, body=[
                                                                               #link{class="btn btn-link", body="<i class='icon-ok'></i> Start-update", postback={start_update, A}, new=false, delegate=common}
                                                                              ]}],
                                             {T, [], TS}
                                     end,
    TD = bm_types:timestamp() - Timestamp,
    #panel{id=Id, body=[
        #panel{class="row-fluid", body=[
                #panel{class="span1", body=FromName},
                #panel{class="span2 offset9", body=[
                                            sugar:format_timedelta(TD),
                                            case Status of
                                                S when S==read; S==sent ->
                                                    #image{image="/img/read.svg",  style="height: 16px;vertical-align:middle;margin-left: 5px;"};
                                                S when S==unread; S==ackwait ->
                                                    #image{image="/img/unread.svg",  style="height: 16px;vertical-align:middle;margin-left: 5px;"};
                                                S when S==new; S==wait_pubkey; S==encrypting ->
                                                    "&nbsp;<i class='icon-mail-forward'></i>"
                                            end
                       ]}
                ], actions=#event{type=click, postback={fold, Record}, delegate=common}},
        #panel{class="row-fluid", body=[
                #panel{class="span12", body=Text}
                ]},
        #panel{class="row-fluid", body=[
                #panel{class="span3 offset4", body=[
                            #link{class="btn btn-link", body=[
                                    #span{class="icon-reply icon-large", text=" "}
                                    ], postback={reply, Subject, From}, new=false},
                            
                        #panel{class="btn-group", body=[
                                #link{ class="btn btn-link droppdown-toggle", body=[
                                        "<i class='icon-reorder icon-large'></i>"
                                        ], new=false, data_fields=[{toggle, "dropdown"}]},
                                #list{numbered=false, class="dropdown-menu pull-right",
                                      body=[
                                        #listitem{body=[
                                                #link{body=[
                                                        "<i class='icon-list-alt icon-large'></i> Archive"
                                                        ], postback={archive, Enc, UID}, new=false}]}
                                        ]}

                                ]}
                        ]}]},
            case Attachments of
                [] ->
                    [];
                A ->
                    [
                    #panel{class="row-fluid", body=[
                            #panel{class="span6", body="<i class='icon-file-alt'></i> Attachment"},
                            #panel{class="span2 offset4", body="<i class='icon-download-alt'></i> Download all"}
                            ]},
                    lists:map(fun(#db_file{id=FID, path=Path, size=Size, date=Date}) ->
                                      {ok, [ #db_file{status=Status} ]} =  db:get_files([ FID ]),
                                      #attachment{fid=FID, filename=Path, size=Size, time=Date, status=Status}
                              end,
                        Attachments)
                        ]
            end

            ]};
render_element(#update_element{enc=Enc, from=From, subject=Subject, text=Data, age=Age, collapse=paragraph}) -> % {{{1
    {Text, Attachments, Timestamp} = case Enc of
               3 ->
                   #message_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
                   {T, A, TS};
               4 ->
                   #task_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
                  {T, A, TS}
           end,
    TD = bm_types:timestamp() - Timestamp,
    [
        #panel{class="row", body=[
                #panel{class="span9", body="<b>Subject: </b>" ++ Subject},
                #panel{class="span2 cell-right", body=sugar:format_timedelta(TD)}
                ]},
        #panel{class="row", body=[
                #panel{class="span12", body=Text}
                ]}
        ].
