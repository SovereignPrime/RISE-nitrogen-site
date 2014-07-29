-module(search).
-compile([export_all]).

-include("db.hrl").
-include("protokol.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

dates(Term) when length(Term) == 1 ->  % {{{1
    NTerm = "0" ++ Term,
    dates(NTerm);
dates(Term) when length(Term) == 2; length(Term) == 4 ->  % {{{1
    db:get_dates(Term);
dates(Term) when length(Term) == 10 ->  % {{{1
    "";
dates(Term) ->  % {{{1
    "".

contacts(Term) ->  % {{{1
    {ok, Contacts} = db:search_contacts(Term),
    case Contacts of
        [] ->
            [];
        Contacts ->
            ["<dl class='dl-horizontal'>",
             "<dt>Contacts:</dt><dd>",
             lists:map(fun(#db_contact{id=Id, name=Name, email=Email}) ->
                               #panel{body=[
                                            #link{text=wf:f("~s (~s)", [Name, Email]),
                                                  postback={to_contact, Id},
                                                  delegate=?MODULE}
                                           ]}
                       end, Contacts),
             "</dd>"]
    end.

groups(Term) ->  % {{{1
    {ok, Groups} = db:search_groups(Term),
    case Groups of
        [] ->
            [];
        Groups ->
    ["<dl class='dl-horizontal'>",
                        "<dt>Groups:</dt><dd>",
                        lists:map(fun(#db_group{id=Id, name=Name}) ->
                                    #panel{body=[
                                            #link{text=Name,
                                                  postback={to_group, Id},
                                                  delegate=?MODULE}
                                            ]}
                            end, Groups),
                         "</dd>"]
    end.

files(Term) ->  % {{{1
    {ok, Files} = db:search_files(Term),
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
                                                             postback={to_file, Id},
                                                             delegate=?MODULE}
                                                      ]}
                                  end, Files),
                        "</dd>"]
    end.

messages(Term) ->  % {{{1
    {ok, Messages} = db:search_messages(Term),
    case Messages of
        [] ->
            [];
        Messages ->
    ["<dl class='dl-horizontal'>",
                        "<dt>Messages:</dt><dd>",
                        lists:map(fun(#message{hash=Id, subject=Subject, from=FID, text=Text}) ->
                                    {ok, #db_contact{name=From}} = db:get_contact_by_address(FID),
                                    #panel{body=[
                                            #link{text=wf:f("~s (~s) ~100s", [
                                                                        Subject,
                                                                        From,
                                                                        Text
                                                                       ]),
                                                  postback={to_message, Id},
                                                  delegate=?MODULE}
                                            ]}
                            end, Messages),
                         "</dd>"]
    end.

tasks(Term) ->  % {{{1
    {ok, Tasks} = db:search_tasks(Term),
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
                                                  delegate=?MODULE}
                                            ]}
                            end, Tasks),
                         "</dd>"]
    end.


%text(Term) ->  % {{{1
%    "".

