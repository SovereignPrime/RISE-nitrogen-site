-module(search).
-compile([export_all]).

-include("db.hrl").
-include("protokol.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

dates(Term, A) when length(Term) == 10 ->
    case lists:dropwhile(fun({date, _}) -> false;
                       (_) ->
                            true
                    end, A) of
        [] -> 
            dates(Term);
        [{date, Date}] ->
            {[{date, DateN}], []} = dates(Term),
            if DateN > Date ->
                   {[{daterange, {Date, DateN}}], []};
               true ->
                   {[{daterange, {DateN, Date}}], []}
            end
    end;
dates(Term, A) ->
    dates(Term).

dates(Term) when length(Term) == 1 ->  % {{{1
    NTerm = "0" ++ Term,
    dates(NTerm);
dates(Term) when length(Term) == 2 ->  % {{{1
    T = wf:to_integer(Term),
    {ok, Dates} = db:search_dates({0, T, T}),
    format_dates(Dates);
dates(Term) when length(Term) == 4 ->  % {{{1
    T = wf:to_integer(Term),
    {ok, Dates}= db:search_dates({T, 0, 0}),
    format_dates(Dates);
dates(Term) when length(Term) == 10 ->  % {{{1
    Date = sugar:date_from_string(Term),
    {ok,  Dates} = db:search_dates(Date),
    {[{date, Date}], []};
dates(Term) ->  % {{{1
    {0, ""}.

contacts(Term) ->  % {{{1
    {ok, Contacts} = db:search_contacts(Term),
    Len = length(Contacts),
    case Contacts of
        [] ->
            {0, []};
        Contacts ->
            {Len, ["<dl class='dl-horizontal'>",
             "<dt>Contacts:</dt><dd>",
             lists:map(fun(#db_contact{id=Id, name=Name, email=Email}) ->
                               #panel{body=[
                                            #link{text=wf:f("~s (~s)", [Name, Email]),
                                                  postback={to_contact, Id},
                                                  delegate=common}
                                           ]}
                       end, Contacts),
             "</dd>"]}
    end.

groups(Term) ->  % {{{1
    {ok, Groups} = db:search_groups(Term),
    Len = length(Groups),
    case Groups of
        [] ->
            {0, []};
        Groups ->
            {Len, ["<dl class='dl-horizontal'>",
                        "<dt>Groups:</dt><dd>",
                        lists:map(fun(#db_group{id=Id, name=Name}) ->
                                    #panel{body=[
                                            #link{text=Name,
                                                  postback={to_group, Id},
                                                  delegate=common}
                                            ]}
                            end, Groups),
                         "</dd>"]}
    end.

files(Files) ->  % {{{1
    case Files of
        [] ->
            [];
        Files ->
            ["<dl class='dl-horizontal'>",
                        "<dt>Files:</dt><dd>",
                        lists:map(fun(#db_file{id=Id,
                                               path=Name,
                                               user=UID,
                                               date=Date,
                                               size=Size}) ->
                                          {ok, #db_contact{name=User}} = db:get_contact(UID),
                                          #panel{body=[
                                                       #link{text=wf:f("~s (~s) - ~s - ~s", [
                                                                                   Name,
                                                                                   sugar:format_file_size(Size),
                                                                                   User,
                                                                                   sugar:date_format(Date)
                                                                                  ]),
                                                             url="/files"}
                                                      ]}
                                  end, Files),
                        "</dd>"]
    end.

messages(Messages) ->  % {{{1
    case Messages of
        [] ->
            [];
        Messages ->
            ["<dl class='dl-horizontal'>",
                        "<dt>Messages:</dt><dd>",
                        lists:map(fun(#message{hash=Id, subject=Subject, from=FID, text=Data}) ->
                                    {ok, #db_contact{name=From}} = db:get_contact_by_address(FID),
                                    #message_packet{text=Text} = binary_to_term(Data),
                                    #panel{body=[
                                            #link{text=wf:f("~s (~s) ~100s", [
                                                                        Subject,
                                                                        From,
                                                                        Text
                                                                       ]),
                                                  postback={to_message, Id},
                                                  delegate=common}
                                            ]}
                            end, Messages),
                         "</dd>"]
    end.

tasks(Tasks) ->  % {{{1
    case Tasks of
        [] ->
            [];
        Tasks ->
            ["<dl class='dl-horizontal'>",
                        "<dt>Tasks:</dt><dd>",
                        lists:map(fun(#db_task{id=Id, name=Subject, text=Text}) ->
                                    #panel{body=[
                                            #link{text=wf:f("~s - ~100s", [
                                                                        Subject,
                                                                        Text
                                                                       ]),
                                                  postback={to_task, Id},
                                                  delegate=common}
                                            ]}
                            end, Tasks),
                         "</dd>"]
    end.

format_dates([]) ->  % {{{1
    {[], ""};
format_dates(Dates) ->  % {{{1
    {[], ["<dl class='dl-horizontal'>",
                        "<dt>Dates:</dt><dd>",
                        lists:foldl(fun(Date, A) ->
                                    A ++ [#panel{body=[
                                            #link{text=sugar:date_format(Date),
                                                 postback={to_date, Date},
                                                 delegate=common}
                                            ]}]
                            end, [], Dates),
                         "</dd>"]}.



term(Term, {OB, OD, OG, OC, OM, OT, OF}) ->
    {LD, D} = search:dates(Term, OB),
    {LG, G} = search:groups(Term),
    {LC, C} = search:contacts(Term),

    {ok, M} = db:search_messages(Term),
    {ok, T} = db:search_tasks(Term),
    {ok, F} = db:search_files(Term),

    {lists:usort(OB ++ []),
     lists:usort(OD ++ D),
     lists:usort(OG ++ G),
     lists:usort(OC ++ C),
     lists:usort(OM ++ M),
     lists:usort(OT ++ T),
     lists:usort(OF ++ F)}.
    
