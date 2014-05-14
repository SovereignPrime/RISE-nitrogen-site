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

name_from_address(Address) ->
    case db:get_contact_by_address(Address) of
        {ok, #db_contact{name=FN}} -> FN;
        {ok, none} -> wf:to_list(Address)
    end.

-spec render_element(#update_element{}) -> body().
%% Render collapsed update {{{1
render_element(#update_element{id=Id,
                               from=From,
                               to=To,
                               text=Data,
                               age=Age,
                               collapse=true,
                               enc=Enc,
                               status=Status}=Record) ->
    FromName = name_from_address(From),
    ToName = name_from_address(To),
    {Text, Attachments, Timestamp} = decode_enc(Enc, Data, true),
    TD = bm_types:timestamp() - Timestamp,
        #panel{id=Id, class="row-fluid", body=[

                #panel{class="span4",
                       body=[
                            "<i class='icon-chevron-down'></i> ",
                            FromName,
                            " <i class='icon-arrow-right'></i> ",
                            ToName]},

                #panel{class="span6", body=io_lib:format("~100s", [Text])},
                #panel{class="span2", body=[
                                            sugar:format_timedelta(TD),
                                            format_status(Status)
                                           ]}
            ], actions=#event{type=click,
                              postback={unfold, Record},
                              delegate=common}};
%% Render uncollapse update  {{{1
render_element(#update_element{id=Id,
                               uid=UID,
                               from=From,
                               to=To,
                               text=Data,
                               age=Age,
                               subject=Subject,
                               collapse=false,
                               enc=Enc,
                               status=Status}=Record) ->
    FromName = name_from_address(From),
    {Text, Attachments, Timestamp} = decode_enc(Enc, Data, false),
    TD = bm_types:timestamp() - Timestamp,
    #panel{id=Id, body=[
        #panel{class="row-fluid", body=[
                #panel{class="span1", body=FromName},
                #panel{class="span2 offset9", body=[
                                            sugar:format_timedelta(TD),
                                            format_status(Status)
                       ]}
                ], actions=#event{type=click, postback={fold, Record}, delegate=common}},
        #panel{class="row-fluid", body=[
                #panel{class="span12", body=Text}
                ]},
        #panel{class="row-fluid", body=[
                #panel{class="span3 offset4", body=[
                            #link{class="btn btn-link", body=[
                                    #span{class="icon-reply icon-large", text=" "}
                                    ], postback={reply, Subject, From}, new=false, delegate=common},
                            
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
                            #panel{class="span6",
                                   body="<i class='icon-file-alt'></i> Attachment"},
                            #panel{class="span2 offset4",
                                   body="<i class='icon-download-alt'></i> Download all"}
                            ]},
                    lists:map(fun(#db_file{id=FID,
                                           path=Path,
                                           size=Size,
                                           date=Date}) ->
                                      {ok, [ #db_file{status=FStatus} ]} =  db:get_files([ FID ]),
                                      #attachment{fid=FID,
                                                  filename=Path,
                                                  size=Size,
                                                  time=Date,
                                                  status=FStatus}
                              end,
                        Attachments)
                        ]
            end

            ]};
%% Render update as single paragraph for relationships {{{1
render_element(#update_element{enc=Enc,
                               from=From,
                               subject=Subject,
                               text=Data,
                               age=Age,
                               collapse=paragraph}) ->
    {Text, Attachments, Timestamp} = decode_enc(Enc, Data, true),
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

format_status(ok) ->  % {{{1
    " (received)";
format_status(read) ->  % {{{1
    " (read)";
format_status(unread) ->  % {{{1
    " (unread)";
format_status(ackwait) ->  % {{{1
    " (sent)";
format_status(new) ->  % {{{1
    " (sending)";
format_status(Status) when Status==wait_pubkey; Status==encrypt_message ->  % {{{1
    " (sending)";
format_status(Status) ->  % {{{1
    " " ++ wf:to_list(Status).

decode_enc(3, Data, _) ->  % {{{1
    try
        #message_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
        {T, A, TS}
    catch
        error:badarg ->
            {"Decoding error", [], bm_types:timestamp()}
    end;
decode_enc(4, Data, true) ->  % {{{1
    #task_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
    {T, A, TS};
decode_enc(4, Data, false) ->  % {{{1
    #task_packet{text=T,
                 due=Due, 
                 involved=Involved,
                 attachments=A,
                 time=TS} = binary_to_term(Data),

    {#panel{ class="", body= [ 
                              #panel{ class="", 
                                      body=["Due: ", Due]},
                              lists:map(fun(#role_packet{address=Address,
                                                         role=R}) ->
                                                {ok, #db_contact{name=Name}} = db:get_contact_by_address(Address),
                                                #panel{ class="",
                                                        body=[Name ++ " - " ++ R]}
                                        end, Involved),
                              T]}, A, TS};
decode_enc(5, Data, true) ->  % {{{1
    #update_packet{text=T, time=TS} = binary_to_term(Data),
    {T, [], TS};
decode_enc(5, Data, false) ->  % {{{1
    #update_packet{text=T, attachments=A, time=TS} = binary_to_term(Data),
    TB = [T, #panel{id=command, 
                    body=[
                          #link{class="btn btn-link",
                                body="<i class='icon-ok'></i> Start-update",
                                postback={start_update, A}, 
                                new=false,
                                delegate=common}
                         ]}],
    {T, [], TS};
decode_enc(_, Data, _) ->  % {{{1
    {wf:to_list( Data ), [], bm_types:timestamp()}.
