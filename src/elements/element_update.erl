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
render_element(#update_element{id=Id, from=From, text=Data, age=Age, collapse=true, enc=Enc}=Record) ->
    {ok, #db_contact{name=FromName}} = db:get_contact_by_address(From),
    Text = case Enc of
               E when E==2; E==3 ->
                   case re:run(Data, "^(.*)\nInvolved:(.*)$", 
                       [{capture, all, binary}, ungreedy, dotall, firstline, {newline, any}]) of
               {match, [_, <<T/bytes>>, <<_InvolvedB/bytes>>]} ->
                   T;
               nomatch ->
                   Data
           end;
               4 ->
                   #task_packet{text=T} = binary_to_term(Data),
                    T
           end,
        #panel{id=Id, class="row-fluid", body=[
                #panel{class="span2", body="<i class='icon-chevron-down'></i> " ++ FromName},
            #panel{class="span8", body=io_lib:format("~100s", [Text])},
                #panel{class="span2", body=sugar:date_format(Age)}
            ], actions=#event{type=click, postback={unfold, Record}}};
render_element(#update_element{id=Id, uid=UID, from=From, to=To, text=Data, age=Age, subject=Subject, collapse=false, attachments=Attachments, enc=Enc}=Record) ->
    {ok, #db_contact{name=FromName}} = db:get_contact_by_address(From),
    Text = case Enc of
               E when E==2; E==3 ->
                   case re:run(Data, "^(.*)\nInvolved:(.*)$", 
                       [{capture, all, binary}, ungreedy, dotall, firstline, {newline, any}]) of
               {match, [_, <<T/bytes>>, <<_InvolvedB/bytes>>]} ->
                   T;
               nomatch ->
                   Data
           end;
               4 ->
                   #task_packet{text=T} = binary_to_term(Data),
                    T
           end,
    #panel{id=Id, body=[
        #panel{class="row-fluid", body=[
                #panel{class="span1", body=FromName},
                #panel{class="span2 offset9", body=sugar:date_format(Age)}
                ], actions=#event{type=click, postback={fold, Record}}},
        #panel{class="row-fluid", body=[
                #panel{class="span12", body=Text}
                ]},
        #panel{class="row-fluid", body=[
                #panel{class="span3 offset4", body=[
                            #link{class="btn btn-link", body=[
                                    #span{class="icon-reply icon-large", text=" "}
                                    ], postback={reply, Subject, To}, new=false},
                            
                        %#span{class="icon-refresh icon-large", text=" "},
                        %#span{class="icon-reorder icon-large"}
                        #panel{class="btn-group", body=[
                                #link{ class="btn btn-link droppdown-toggle", body=[
                                        "<i class='icon-reorder icon-large'></i>"
                                        ], new=false, data_fields=[{toggle, "dropdown"}]},
                                #list{numbered=false, class="dropdown-menu pull-right",
                                      body=[
                                        #listitem{body=[
                                                #link{body=[
                                                        "<i class='icon-list-alt icon-large'></i> Archive"
                                                        ], postback={archive, UID}, new=false}]}
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
                        Attachments
                        ]
            end

            ]};
render_element(#update_element{from=From, text=Text, age=Age, collapse=paragraph}) ->
    [
        #panel{class="row", body=[
                #panel{class="span9", body="<b>Subject: </b>" ++ From},
                #panel{class="span2 cell-right", body=sugar:date_format(Age)}
                ]},
        #panel{class="row", body=[
                #panel{class="span12", body=Text}
                ]}
        ].
