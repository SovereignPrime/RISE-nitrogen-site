-module(search).
-compile([export_all]).

-include("db.hrl").
-include("protokol.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

dates_if(Terms) ->  % {{{1
    case dict:is_key("Daterange", Terms) of
        true ->
            wf:info("Test~n"),
            {Terms, []};
        false ->
            Term = get_term(Terms),
            case dict:find("Date", Terms) of
                error ->
                    case  dates(Term) of
                        {[], Ts} ->
                            {Terms, Ts};
                        {Date, Ts} ->
                            {dict:erase("Term", dict:append_list("Date", Term, Terms)), Ts}
                    end;
                {ok, Date} ->
                    case  dates(Term) of
                        {[], Ts} ->
                            {Terms, Ts};
                        {DateN, Ts} ->
                            ADict1 = dict:erase("Date", Terms),
                            DT = sugar:date_from_string(Date),
                            case lists:usort([DT, DateN]) of
                                L when length(L) == 1->
                                    {dict:erase("Term", dict:append_list("Date", Date, ADict1)), []};
                                L ->
                                    {dict:erase("Term", dict:append_list("Daterange", list_to_tuple(L), ADict1)), []}
                            end
                    end
            end
    end.

dates(Term) when length(Term) == 1 ->  % {{{1
    NTerm = "0" ++ Term,
    dates(NTerm);
dates(Term) when length(Term) == 2 ->  % {{{1
    try
        T = wf:to_integer(Term),
        {ok, Dates} = db:search_dates({0, T, T}),
        {[], Dates}
    catch 
        error:badarg ->
            {[], []}
    end;
dates(Term) when length(Term) == 4 ->  % {{{1
    try
        T = wf:to_integer(Term),
        {ok, Dates}= db:search_dates({T, 0, 0}),
        {[], Dates}
    catch 
        error:badarg ->
            {[], []}
    end;
dates(Term) when length(Term) == 10 ->  % {{{1
    try
        Date = sugar:date_from_string(Term),
        {ok,  Dates} = db:search_dates(Date),
        {Date, []}
    catch 
        error:badarg ->
            {[], []}
    end;
dates(Term) ->  % {{{1
    {[], ""}.

contacts(Terms) ->  % {{{1
    Term = get_term(Terms),
    case dict:is_key("Group", Terms) of
        false ->
            {ok, Contacts} = db:search_contacts(Term),
            case Contacts of
                [] ->
                    {Terms, []};
                Contacts ->
                    case lists:keyfind(Term, #db_contact.name, Contacts) of
                        false ->
                            {Terms, #panel{body=["<dl class='dl-horizontal'>",
                                                 "<dt>Contacts:</dt><dd>",
                                                 lists:map(fun(#db_contact{id=Id,
                                                                           name=Name,
                                                                           email=Email}) ->
                                                                   #panel{body=[
                                                                                #link{text=wf:f("~s (~s)", [Name, Email]),
                                                                                      postback={search, Name},
                                                                                      delegate=common}
                                                                               ]}
                                                           end, Contacts),
                                                 "</dd>"]}};
                        #db_contact{name=Term} ->
                            {dict:erase("Term", dict:append_list("Contact", Term, Terms)), []}
                    end
            end;
        _ ->
            {Terms, []}
    end.

groups(Terms) ->  % {{{1
    Term = get_term(Terms),
    case {dict:is_key("Group", Terms), dict:is_key("Contact", Terms)} of
        {false, false} ->
            {ok, Groups} = db:search_groups(Term),
            case Groups of
                [] ->
                    {Terms, []};
                Groups ->
                    case lists:keyfind(Term, #db_group.name, Groups) of
                        false ->
                            {Terms, #panel{body=["<dl class='dl-horizontal'>",
                                 "<dt>Groups:</dt><dd>",
                                 lists:map(fun(#db_group{id=Id, name=Name}) ->
                                                   #panel{body=[
                                                                #link{text=Name,
                                                                      postback={search, Name},
                                                                      delegate=common}
                                                               ]}
                                           end, Groups),
                                 "</dd>"]}};
                        #db_group{name=Term} ->
                            {dict:erase("Term", dict:append_list("Group", Term, Terms)), []}
                    end
            end;
        _ ->
            {Terms, []}
    end.

files(Files) ->  % {{{1
    case Files of
        [] ->
            [];
        Files ->
            ["<dl class='dl-horizontal'>",
                        "<dt>Files:</dt><dd>",
                        lists:map(fun(#bm_file{hash=Id,
                                               name=Name,
                                               time=Date,
                                               size=Size}) ->
                                          Users = element_file_row:get_file_senders(Id),
                                          #panel{body=[
                                                       #link{text=wf:f("~s (~s) - ~s - ~s", [
                                                                                   Name,
                                                                                   sugar:format_file_size(Size),
                                                                                   Users,
                                                                                   sugar:date_format(Date)
                                                                                  ]),
                                                             url="/files"}
                                                      ]}
                                  end, Files),
                        "</dd>"]
    end.

messages(Messages) ->  % {{{1
    M = lists:foldl(fun(#message{hash=Id, subject=Subject, from=FID, text=Data}, A) ->
                          {ok,
                           #db_contact{name=From}} = db:get_contact_by_address(FID),
                          try binary_to_term(Data) of
                              #message_packet{text=Text} ->
                                  A ++ [#panel{body=[
                                                     #link{text=wf:f("~s (~s) ~100s",
                                                                     [
                                                                      Subject,
                                                                      From,
                                                                      Text
                                                                     ]),
                                                           postback={to_message,
                                                                     Id},
                                                           delegate=common}
                                                    ]}];
                              _ ->
                                  A
                          catch
                              error:_ ->
                                  A
                          end
                    end,
                    [],
                    Messages),
    case M of
        [] ->
            [];
        M ->
            ["<dl class='dl-horizontal'>",
                        "<dt>Messages:</dt><dd>",
                        M,
                         "</dd>"]
    end.

tasks(Messages) ->  % {{{1
    error_logger:info_msg("Messages for tasks: ~p~n", [Messages]),
    Tasks = lists:foldl(fun(#message{hash=Id,
                                     subject=Subject,
                                     from=FID,
                                     text=Data}, A) ->
                                {ok,
                                 #db_contact{name=From}} = db:get_contact_by_address(FID),
                                try binary_to_term(Data) of
                                    #task_packet{id=ID, name=Subject, text=Text} ->
                                        A ++ [#panel{
                                                 body=[
                                                       #link{text=wf:f("~s - ~100s",
                                                                       [
                                                                        Subject,
                                                                        Text
                                                                       ]),
                                                             postback={to_task,
                                                                       ID},
                                                             delegate=common}
                                                      ]}];
                                    _ ->
                                        []
                                catch
                                    error:_ -> []
                                end
                        end,
                        [],
                        Messages),
    error_logger:info_msg("Tasks: ~p~n", [Tasks]),
    case Tasks of
        [] ->
            [];
        Tasks ->
            ["<dl class='dl-horizontal'>",
             "<dt>Tasks:</dt><dd>",
             Tasks,
             "</dd>"]
    end.

format_dates([]) ->  % {{{1
    "";
format_dates(Dates) ->  % {{{1
    ["<dl class='dl-horizontal'>",
                        "<dt>Dates:</dt><dd>",
                        lists:foldl(fun(Date, A) ->
                                    A ++ [#panel{body=[
                                            #link{text=sugar:date_string(Date),
                                                 postback={search, sugar:date_string(Date)},
                                                 delegate=common}
                                            ]}]
                            end, [], Dates),
                         "</dd>"].



terms(Terms) ->  % {{{1
    {GTerms, G} = search:groups(Terms),
    {CTerms, C} = search:contacts(GTerms),
    {DTerms, D} = search:dates_if(CTerms),

    {ok, M} = db:search_messages(DTerms),
    {ok, F} = db:search_files(DTerms),

    error_logger:info_msg("Messages: ~p~n", [M]),
    {DTerms,
     [
      format_dates(lists:usort(D)),
      G, C,
      messages(lists:usort(M)),
      tasks(lists:usort(M)),
      files(lists:usort(F))
     ]}.

get_term(Terms) ->  % {{{1
   case dict:find("Term", Terms) of
        error ->
            "";
        {ok, T} ->
            T
    end.

check_roles(Terms, True, False) ->  % {{{1
    Roles = [R || {R, _} <- ?ROLES],
    case lists:any(fun(R) -> dict:is_key(R, Terms) end, Roles) of
        true ->
            True();
        false ->
            False()
    end.
