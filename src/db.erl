-module(db).
-compile([export_all]).
-include("db.hrl").

install()->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    {atomic, ok} = mnesia:create_table(db_group, [{disc_copies, [node()]}, {attributes, record_info(fields, db_group)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_contact, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_task, [{disc_copies, [node()]}, {attributes, record_info(fields, db_task)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_file, [{disc_copies, [node()]}, {attributes, record_info(fields, db_file)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_expense, [{disc_copies, [node()]}, {attributes, record_info(fields, db_expense)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_update, [{disc_copies, [node()]}, {attributes, record_info(fields, db_update)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_contact_roles, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact_roles)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_attachment, [{disc_copies, [node()]}, {attributes, record_info(fields, db_attachment)}, {type, ordered_set}]).


new_task(Name, Due, Text, Parent) ->
    {atomic, ok} = mnesia:transaction(fun() ->
                    N = case mnesia:table_info(db_task, size) of
                        '$end_of_table' ->
                            0;
                        A -> 
                            A
                    end,
                    Task = #db_task{id=N+1,
                                    name=Name,
                                    due=Due,
                                    text=Text,
                                    parent=Parent,
                                    status=new
                                   },
                    ok = mnesia:write(Task)
        end).
save_task(Id, Name, Due, Text, Parent, Status) ->
    Task = #db_task{id=Id,
                    name=Name,
                    due=Due,
                    text=Text,
                    parent=Parent,
                    status=Status
                   },
    io:format("Task to save: ~p~n", [Task]),
    {atomic, ok} = mnesia:transaction(fun() ->
                ok = mnesia:write(Task)
        end).

new_expense(Name, Due, Text, Amount) ->
    {atomic, ok} = mnesia:transaction(fun() ->
                    N = case mnesia:table_info(db_expense, size) of
                        '$end_of_table' ->
                            0;
                        A -> 
                            A
                    end,
                    Task = #db_expense{id=N+1,
                                       name=Name,
                                       date=Due,
                                       text=Text,
                                       amount=Amount,
                                       status=new
                                      },
                    ok = mnesia:write(Task)
            end).
new_update(Subject, Text) ->
    {atomic, ok} = mnesia:transaction(fun() ->
                    N = case mnesia:table_info(db_update, size) of
                        '$end_of_table' ->
                            0;
                        A -> 
                            A
                    end,
                    Task = #db_update{id=N+1,
                                      date=now(),
                                      from="Me",
                                      subject=Subject,
                                      text=Text,
                                      status=new
                                     },
                    ok = mnesia:write(Task)
            end).
get_task() ->
    transaction(fun() ->
                mnesia:read(db_task, mnesia:last(db_task))
            end).

get_task(Id) -> 
    transaction(fun() ->
                mnesia:read(db_task, Id)
            end).
%%%
%% Attachment routines
%%%

get_attachments(Record) ->
    Type = element(1, Record),
    Id = element(2, Record),
     transaction(fun() ->
                mnesia:select(db_attachment, [{#db_attachment{ type=Type, tid=Id, _='_'}, [], ['$_']}])
            end).

save_attachments(Record, Filename) ->
    Type = element(1, Record),
    Id = element(2, Record),
     transaction(fun() ->
                N = case mnesia:table_info(db_attachment, size) of
                        '$end_of_table' ->
                            0;
                        A -> 
                            A
                    end,
                mnesia:write(#db_attachment{id=N+1, file=Filename, type=Type, tid=Id})
            end).

%%%
%%  Admin functions
%%%
    
all_tasks() ->
    transaction(fun() ->
                mnesia:match_object(#db_task{_='_'})
            end).
all_expenses() ->
    transaction(fun() ->
                mnesia:match_object(#db_expense{_='_'})
            end).

all_updates() ->
    transaction(fun() ->
                mnesia:match_object(#db_update{_='_'})
            end).

all_attachments() ->
    transaction(fun() ->
                mnesia:match_object(#db_attachment{_='_'})
            end).

get_involved(Id) ->
    transaction(fun() ->
                R = mnesia:match_object(#db_contact_roles{type=task, tid=Id, _='_'}),
                lists:map(fun(#db_contact_roles{role=Role, contact=Contact}) ->
                            #db_contact{name=Name, email=Email} = mnesia:read(Contact),
                            {Name, Role}
                    end, R)
        end).


get_users(N) ->
    transaction(fun() ->
                mnesia:select(db_contact, [{#db_contact{name='$1'}, [], ['$1']}], N, read)
        end).

get_tasks(N) ->
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{_='_'}, [], ['$_']}], N, read)
            end).

get_tasks(C, _N) ->
    transaction(fun() ->
                mnesia:select(C)
        end).
 
%%%
%% Account routines
%%%

get_account(User, Passwd) ->
    transaction(fun() ->
                mnesia:match_object(db_contact, #db_contact{email=User, my=Passwd, _='_'}, read)
        end).

create_account(User, Passwd) ->
    transaction(fun() ->
                case mnesia:match_object(db_contact, #db_contact{email=User, my=Passwd, _='_'}, write) of
                    [] -> 
                        N = case mnesia:table_info(db_task, size) of
                            '$end_of_table' ->
                                0;
                            A -> 
                                A
                        end,
                        mnesia:write(#db_contact{id=N+1,
                                                 name="My",
                                                 email=User,
                                                 my=Passwd
                                                }),
                        mnesia:read(db_contact, N+1);
                    [U] ->
                        U
                end
        end).

%%%
%% Transaction helper
%%%

transaction(Fun) ->
   case mnesia:transaction(Fun) of
        {error, R} ->
            {error, R};
        {atomic, '$end_of_table'} ->
            {ok, [], undefined};
        {atomic, R} ->
            {ok, R}
    end.
