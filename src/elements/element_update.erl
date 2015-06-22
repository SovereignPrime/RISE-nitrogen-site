%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_update).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("protokol.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, update_element).

name_from_address(Address) ->  % {{{1
    case db:get_contact_by_address(Address) of
        {ok, #db_contact{name=FN}} -> FN;
        {ok, none} -> wf:to_list(Address)
    end.

-spec render_element(#update_element{}) -> body().
%% Render collapsed update {{{1
render_element(#update_element{id=Id,
                               message=#message{
                                          from=From,
                                          to=To,
                                          text=Data,
                                          enc=Enc,
                                          status=Status}=Message,
                               age=Age,
                               collapse=true
                               }=Record) ->
    FromName = name_from_address(From),
    ToName = name_from_address(To),
    {Text, Timestamp, _} = decode_enc(Enc, Data, true),
    TD = bm_types:timestamp() - Timestamp,
        #panel{id=Id, class="row-fluid clickable", body=[

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
                               message=#message{
                                          hash=UID,
                                          from=From,
                                          to=To,
                                          text=Data,
                                          subject=Subject,
                                          enc=Enc,
                                          attachments=Attachments,
                                          status=Status
                                         } = Message,
                               age=Age,
                               collapse=false
                              }=Record) ->
    FromName = name_from_address(From),
    ToName = name_from_address(To),
    NStatus = case db:set_read(UID) of
                  {ok, unread} ->
                      index:replace_left(),
                      wf:replace(count, common:unread()),
                      read;
                  {ok, _} ->
                      Status
              end,
    {Text, Timestamp, TID} = decode_enc(Enc, Data, false),
    TD = bm_types:timestamp() - Timestamp,
    #panel{id=Id,
           body=[
                 #panel{class="row-fluid",
                        body=[
                              #panel{class="span4",
                                     body=[
                                           "<i class='icon-chevron-down'></i> ",
                                           FromName,
                                           " <i class='icon-arrow-right'></i> ",
                                           ToName]},
                              #panel{class="span2 offset6",
                                     body=[
                                           sugar:format_timedelta(TD),
                                           format_status(NStatus)
                                          ]}
                             ],
                        actions=#event{type=click,
                                       postback={fold, Record},
                                       delegate=common}},
                 #panel{class="row-fluid",
                        body=[
                              #panel{class="span12",
                                     body=Text}
                             ]},
                 #panel{class="row-fluid",
                        body=[
                              #panel{class="span3 offset4",
                                     body=[
                                           #link{class="btn btn-link",
                                                 body=[
                                                       #span{class="icon-reply icon-large",
                                                             text=" "}
                                                      ],
                                                 postback=case Enc of

                                                              E when E == 3; E == 2 -> 
                                                                  {reply, Subject, From};
                                                              4 ->
                                                                  {to_task, TID}
                                                          end,
                                                 new=false,
                                                 delegate=common},

                                           #panel{class="btn-group",
                                                  body=[
                                                        #link{ class="btn btn-link droppdown-toggle",
                                                               body=[
                                                                     "<i class='icon-reorder icon-large'></i>"
                                                                    ],
                                                               new=false,
                                                               data_fields=[{toggle, "dropdown"}]},
                                                        #list{numbered=false,
                                                              class="dropdown-menu pull-right",

                                                              body=[
                                                                    #listitem{body=[
                                                                                    #link{body=[
                                                                                                "<i class='icon-list-alt icon-large'></i> Archive"
                                                                                               ],
                                                                                          postback={archive, Enc, UID},
                                                                                          new=false}]}
                                                                   ]}

                                                       ]}
                                          ]}]},
                 case Attachments of
                     undefined ->
                         [];
                     [] ->
                         [];
                     A ->
                         {ok, Files} = db:get_files(A),
                         [
                          #panel{class="row-fluid", body=[
                                                          #panel{class="span6",
                                                                 body="<i class='icon-file-alt'></i> Attachment"},
                                                          #panel{class="span2 offset4",
                                                                 body="<i class='icon-download-alt'></i> Download all"}
                                                         ]},
                          lists:map(fun(#bm_file{
                                           hash=FID,
                                           name=Path,
                                           size=Size,
                                           time={Date, _Time},
                                           status=FStatus}) ->
                                            #attachment{fid=FID,
                                                        filename=Path,
                                                        size=Size,
                                                        time=Date,
                                                        status=FStatus}
                                    end, Files)
                         ]
                 end

                ]};
%% Render update as single paragraph for relationships {{{1
render_element(#update_element{
                  id=Id,
                  message=#message{
                             hash=UID,
                             from=From,
                             to=To,
                             text=Data,
                             subject=Subject,
                             enc=Enc,
                             attachments=Attachments,
                             status=Status
                            } = Message,
                  age=Age,
                  collapse=paragraph}) ->
    {Text, Timestamp, _} = decode_enc(Enc, Data, true),
    TD = bm_types:timestamp() - Timestamp,
    [
     #panel{class="row-fluid",
            body=[
                  #panel{style="cursor:pointer",
                        class="span9",
                         body="<b>Subject: </b>" ++ Subject},
                  #panel{class="span2 cell-right",
                         body=sugar:format_timedelta(TD)}
                 ],
            actions=#event{type=click,
                           postback={to_message, Id},
                           delegate=common }},
     #panel{class="row-fluid",
            body=[
                  #panel{class="span12",
                         body=Text}
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
    " (doing PoW)";
format_status(wait_pubkey) ->  % {{{1
    " (waiting for key)";
format_status(encrypt_message) ->  % {{{1
    " (sending)";
format_status(Status) ->  % {{{1
    " " ++ wf:to_list(Status).

decode_enc(Enc, Data, Collapsed) when Enc == 2; Enc == 3 ->  % {{{1
    try binary_to_term(Data) of
        #message_packet{text=T, time=TS} ->
            Text = ?WF_IF(Collapsed, wf:html_encode(T), wf:html_encode(T, whites)),
            {Text, TS, empty};
        #update_packet{text=T, time=TS} ->
            EncodedT = wf:html_encode(T, whites),
            TB = [EncodedT, #panel{id=command, 
                                   body=[
                                         #link{class="btn btn-link",
                                               body="<i class='icon-ok'></i> Start-update",
                                               %postback={start_update, A}, 
                                               new=false,
                                               delegate=common}
                                        ]}],
            Text = ?WF_IF(Collapsed, wf:html_encode(T), wf:html_encode(T, whites)),
            {Text, TS, empty};
        Task ->
            #task_packet{id=Id,
                         text=T,
                         due=Due, 
                         involved=Involved,
                         status=Status,
                         time=TS} = receiver:extract_task(Data),
            Body = #panel{ class="",
                           body= [ 
                                  #panel{class="", 
                                         body=["Due: ", Due]
                                        },
                                  #panel{class="", 
                                         body=["Status: ", Status]
                                        },
                                  lists:map(fun(#role_packet{address=Address,
                                                             role=R}) ->
                                                    {ok,
                                                     #db_contact{name=Name}} = db:get_contact_by_address(Address),
                                                    #panel{class="",
                                                           body=[Name ++ " - " ++ R]}
                                            end,
                                            Involved),
                                  #br{},
                                  wf:html_encode(T, whites)
                                 ]},
            Text = ?WF_IF(Collapsed, wf:html_encode(T, whites), Body),
            {Text, TS, Id}
    catch
        error:badarg ->
            {"Decoding error", bm_types:timestamp(), empty}
    end;
%decode_enc(5, Data, false) ->  % {{{1
%     = binary_to_term(Data),
%    EncodedT = wf:html_encode(T, whites),
%    TB = [EncodedT, #panel{id=command, 
%                           body=[
%                                 #link{class="btn btn-link",
%                                       body="<i class='icon-ok'></i> Start-update",
%                                       %postback={start_update, A}, 
%                                       new=false,
%                                       delegate=common}
%                                ]}],
%    {EncodedT, TS, empty};
decode_enc(_, Data, _) ->  % {{{1
    {wf:html_encode(wf:to_list( Data ), whites), bm_types:timestamp(), empty}.
