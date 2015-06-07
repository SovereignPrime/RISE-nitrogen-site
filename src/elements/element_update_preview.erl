%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_update_preview).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
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
                               message=#message{
                                          hash=UID,
                                          from=From,
                                          to=To,
                                          subject=Subject,
                                          text=Data,
                                          status=Status},
                               flag=Flag,
                               archive=Archive}) -> 
    {Text, Timestamp, Icon} = try binary_to_term(Data, []) of
                             #message_packet{text=Txt,
                                             time=TS} ->
                                 {Txt, TS, 3};
                             Task ->
                                 #task_packet{text=Txt,
                                              time=TS} = receiver:extract_task(Task),
                                 {Txt, TS, 4};
                             #update_packet{text=Txt, time=TS} -> 
                                 {Txt, TS, 5};
                             _ ->
                                 {<<"Decoding error! Data: ", Data/bytes>>, bm_types:timestamp()}
                         catch
                             error:badarg ->
                                 {<<"Decoding error! Data: ", Data/bytes>>, bm_types:timestamp()}
                         end,
    TD = bm_types:timestamp() - Timestamp,
    CurrentId = wf:session(current_update_id),
    HasCurrent = lists:any(fun(I) -> (I == CurrentId) end, sugar:maybe_wrap_list(UID)),
    Class = if HasCurrent ->
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
                          postback={selected, sugar:maybe_wrap_list(UID), Subject, Archive}}}.

render_unread_icon(unread) ->  % {{{1
    "<i class='icon icon-sign-blank'></i>";
render_unread_icon(_) ->  % {{{1
    "".

render_icon(Icon) when Icon==1; Icon==2 ->  % {{{1
    "<i class='icon-envelope'></i>";
render_icon(3) ->  % {{{1
    "<i class='icon-envelope'></i>";
render_icon(4) ->  % {{{1
   #image{image="/img/tasks.svg",
          class="icon",
          style="height:16px;vertical-align:middle;"
   };
render_icon(5) ->  % {{{1
    "<i class='icon-refresh'></i>".

participant_list(List) ->  % {{{1
    Deduped = common:remove_duplicates(lists:flatten(List)),
    wf:join([get_name(Address) || Address <- Deduped], ", ").

get_name(UID) ->  % {{{1
    case db:get_contact_by_address(UID) of
        {ok, #db_contact{name=FN}} ->
            FN;
        {ok, none} ->
            "Anonymous"
            %wf:to_list(UID)
    end.
