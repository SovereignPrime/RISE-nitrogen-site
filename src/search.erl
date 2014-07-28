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
                        lists:map(fun(#db_file{id=Id, path=Name, size=Size}) ->
                                    #panel{body=[
                                            #link{text=Name,
                                                  postback={to_file, Id},
                                                  delegate=?MODULE}
                                            ]}
                            end, Files),
                         "</dd>"]
    end.

text(Term) ->  % {{{1
    "".

