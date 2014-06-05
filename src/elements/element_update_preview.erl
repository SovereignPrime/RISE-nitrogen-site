%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_update_preview).
-include_lib("nitrogen_core/include/wf.hrl").
-include("protokol.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
         reflect/0,
         render_element/1,
         render_icon/1
        ]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, update_preview).


-spec render_element(#update_preview{}) -> body().
%% Render Messages  or  update  {{{1
render_element(#update_preview{id=Id,
                               icon=Icon,
                               from=From,
                               to=To,
                               age=Age,
                               subject=Subject,
                               text=Data,
                               flag=Flag,
                               status=Status,
                               archive=Archive}) when Icon==1; Icon==2; Icon==3; Icon==5 -> 
    {Text, Attachments, Timestamp} = case Icon of
               T when T==1; T==2 ->
                     {Data, [], bm_types:timestamp()};
               3 ->
                   try
                       #message_packet{text=T,
                                       attachments=A,
                                       time=TS} = binary_to_term(Data, []),
                       {T, A, TS}
                   catch
                       error:badarg ->
                           {"Decoding error", [], bm_types:timestamp()}
                   end;


               5 ->
                   try
                       #update_packet{text=T, time=TS} = binary_to_term(Data, []),
                       {T, [], TS}
                   catch
                       error:badarg ->
                           {"Decoding error", [], bm_types:timestamp()}
                   end
           end,
    TD = bm_types:timestamp() - Timestamp,
    CurrentId = wf:session(current_update_id),
    Class = if Id == CurrentId ->
           "current";
       true ->
           ""
    end,

    #panel{class=['update-preview',clickable,Class],
           style="line-height:18px;margin-top:18px;",
           body=[
                 #panel{class="row-fluid no-padding",
                        body=[

                              #panel{class="span1 no-padding",body=render_icon(Icon)},
                              #panel{class='span9 no-padding update-participant-list',
                                     text=participant_list([From] ++ [To])},
                              #panel{class='span2 cell-right no-padding update-age',
                                     body=[sugar:format_timedelta(TD)]}
                             ]},
                 case Subject of 
                     undefined -> "";
                     Subject ->
                         #panel{class='row-fluid',
                                body=[
                                      #panel{class='span11 offset1 no-padding',
                                             style="overflow: hidden; font-weight:bold",
                                             text=Subject}
                                     ]}
                 end,
                 #panel{class='row-fluid',
                        body=[
                              if Flag == true ->
                                     [
                                      #panel{class='span1', body=[
                                        render_unread_icon(Status)
                                      ]},
                                      #panel{class='span11 shorten-text',
                                             style="-webkit-line-clamp:2;",
                                             text=[Text]}
                                     ];
                                 true ->
                                     #panel{class='span12 shorten-text',
                                            style="-webkit-line-clamp:2;",
                                            text=[Text]}
                              end
                             ]}
                ],
           actions=#event{type=click,
                          postback={selected, Id, Subject, Archive}}};

%% Render tasks {{{1
render_element(#update_preview{id=Id,
                               icon=Icon,
                               from=From,
                               to=To,
                               age=Age,
                               subject=Subject,
                               text=Data,
                               flag=Flag,
                               status=Status,
                               archive=Archive}) when Icon==4 ->  

    #task_packet{text=Text,
                 time=Timestamp} = try
                                       receiver:extract_task(Data)
                                   catch
                                       error:badarg ->
                                           {"Decoding error", [], bm_types:timestamp()}
                                   end,
    TD = bm_types:timestamp() - Timestamp,
    CurrentId = wf:session(current_update_id),
    Class = if Id == CurrentId ->
           "current";
       true ->
           ""
    end,
    #panel{class=['update-preview',Class],
           style="line-height:18px;margin-top:18px;",
           body=[
                 #panel{class="row-fluid no-padding",
                        body=[
                              #panel{class="span1 no-padding",body=render_icon(Icon)},
                              #panel{class='span9 no-padding update-participant-list',
                                     text=participant_list([From] ++ [To])},
                              #panel{class='span2 cell-right no-padding update-age',
                                     body=[sugar:format_timedelta(TD)]}
                             ]},
                 case Subject of 
                     undefined -> "";
                     Subject ->
                         #panel{class='row-fluid',
                                body=[
                                      #panel{class='span11 offset1 no-padding',
                                             style="overflow: hidden;font-weight:bold",
                                             text=[Subject]}
                                     ]}
                 end,
                 #panel{class='row-fluid',
                        body=[
                              if Flag == true ->
                                     [
                                      #panel{class='span1',style="text-align:center", body=[
                                        render_unread_icon(Status)
                                      ]},
                                      #panel{class='span11 shorten-text',
                                             style="-webkit-line-clamp:2; height:2.7em;",
                                             text=[Text]}
                                     ];
                                 true ->
                                     #panel{class='span12 shorten-text',
                                            style="height:2.7em; -webkit-line-clamp:2;",
                                            text=[Text]}
                              end
                                                ]}
                ],
           actions=#event{type=click,
                          postback={selected, Id, Subject, Archive}}}.


render_unread_icon(unread) ->
    "<i class='icon-square'></i>";
render_unread_icon(_) ->
    "".

render_icon(Icon) when Icon==1; Icon==2 ->
    "<i class='icon-envelope'></i>";
render_icon(3) ->
    "<i class='icon-envelope'></i>";
render_icon(4) ->
   #image{image="/img/tasks.svg",
          class="icon",
          style="height:16px;vertical-align:middle;"
   };
render_icon(5) ->
    "<i class='icon-refresh'></i>".

participant_list(List) ->
    Deduped = common:remove_duplicates(lists:flatten(List)),
    wf:join([get_name(Address) || Address <- Deduped], ", ").

get_name(UID) ->
    case db:get_contact_by_address(UID) of
        {ok, #db_contact{name=FN}} ->
            FN;
        {ok, none} ->
            "Anonymous"
            %wf:to_list(UID)
    end.
