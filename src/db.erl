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
    {atomic, ok} = mnesia:create_table(db_group_members, [{disc_copies, [node()]}, {attributes, record_info(fields, db_group_members)}, {type, bag}]),
    {atomic, ok} = mnesia:create_table(db_attachment, [{disc_copies, [node()]}, {attributes, record_info(fields, db_attachment)}, {type, ordered_set}]).


%%%
%% Get new id fot Type
%%%

next_id(Type) ->
    case mnesia:table_info(Type, size) of
        '$end_of_table' ->
            1;
        A -> 
            A+1
    end.
%%%
%% General routines
%%
save(Contact) ->
    transaction(fun() ->
                mnesia:write(Contact)
        end).
%%%
%% Task routines
%%%

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

save_subtask(Id, PId) ->
    transaction(fun() ->
                [ C ] = mnesia:wread({ db_task, Id }),
                mnesia:write(C#db_task{parent=PId})
        end).

get_task() ->
    transaction(fun() ->
                mnesia:read(db_task, mnesia:last(db_task))
            end).

get_task(Id) -> 
    transaction(fun() ->
                mnesia:read(db_task, Id)
            end).

get_tasks_by_user(UID) ->
    transaction(fun() ->
                Tasks = mnesia:match_object(#db_contact_roles{contact=UID, type=db_task, _='_'}),
                iterate(db_task, Tasks, fun(_, #db_contact_roles{tid=I, role=Role}) ->
                            [Task] = mnesia:read(db_task, I),
                            Task#db_task{parent=Role}
                    end)
        end).

get_tasks(Parent) ->
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{parent=Parent, _='_'}, [], ['$_']}])
            end).

get_tasks(C, _N) ->
    transaction(fun() ->
                mnesia:select(C)
        end).
%%%
%% Expense routines
%%%

save_expense(Id, Name, Due, Text, Amount, #db_contact{id=From}, #db_contact{id=To}) ->
    {atomic, ok} = mnesia:transaction(fun() ->
                    Task = #db_expense{id=Id,
                                       name=Name,
                                       date=Due,
                                       text=Text,
                                       amount=Amount,
                                       from=From,
                                       to=To,
                                       status=new
                                      },
                    ok = mnesia:write(db_expense, Task, write)
            end).

%%%
%% Updates routines
%%%

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

get_updates_by_user(UID) ->
    transaction(fun() ->
                mnesia:match_object(#db_update{from=UID, _='_'})
        end).

%%%
%% Contact routines
%%%

get_contact(undefined) ->
    {ok, #db_contact{}};
get_contact(Id) ->
    transaction(fun() ->
                [U] = mnesia:read(db_contact, Id),
                U
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

add_user_to_group(Group, User) ->
    transaction(fun() ->
                mnesia:write(#db_group_members{group=Group, contact=User})
        end).
get_contacts_by_group(all) ->
    all_contacts();
get_contacts_by_group(Group) ->
    transaction(fun() ->
                G = mnesia:select(db_group, [{#db_group{id='$1', subgroups=Group, _='_'}, [], ['$1']}]),
                U = iterate(db_group_members, [Group | G]),
                iterate(db_contact, U, fun(Type, #db_group_members{contact=Id}) ->
                            mnesia:read(Type, Id)
                    end)
        end).

get_groups_for_user(UID) ->
    transaction(fun() ->
                GIDS = mnesia:select(db_group_members, [{#db_group_members{group='$1', contact=UID}, [], ['$1']}]),
                iterate(db_group, GIDS)
        end).

%%%
%% Group routines
%%%

get_groups() ->
    transaction(fun() ->
                get_subgroup(undefined)
        end).

update_group_name(Id, Name) ->
    transaction(fun() ->
                [G] = mnesia:read(db_group, Id),
                mnesia:write(G#db_group{id=Id, name=Name})
        end).
save_subgroup(Id, Parent) ->
    transaction(fun() ->
                [G] = mnesia:wread({db_group, Id}),
                mnesia:write(G#db_group{subgroups=Parent})
        end).
%%%
%% File routines
%%%

save_file(Name, Path, #db_contact{id=UID}) ->
    Size = filelib:file_size(Path),
    Type = filename:extension(Name),
    transaction(fun() ->
                mnesia:write(#db_file{id=filename:basename(Path), 
                                      path=Name, 
                                      size=Size,
                                      date=now(),
                                      status=uploaded,
                                      user=UID,
                                      type=Type
                                     })
        end).

%%%
%% Attachment routines
%%%

get_attachments(Record) ->
    Type = element(1, Record),
    Id = element(2, Record),
     transaction(fun() ->
                A = mnesia:select(db_attachment, [{#db_attachment{ file='$1', type=Type, tid=Id, _='_'}, [], ['$1']}]),
                iterate(db_file, A)
            end).

save_attachments(Record, Files) ->
    Type = element(1, Record),
    Id = element(2, Record),
     transaction(fun() ->
                save_attachment(Type, Id, Files, db:next_id(db_attachment))
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

all_memberss() ->
    transaction(fun() ->
                mnesia:match_object(#db_group_members{_='_'})
            end).

all_attachments() ->
    transaction(fun() ->
                mnesia:match_object(#db_attachment{_='_'})
            end).

all_files() ->
    transaction(fun() ->
                mnesia:match_object(#db_file{_='_'})
            end).

all_groups() ->
    transaction(fun() ->
                mnesia:match_object(#db_group{_='_'})
            end).
 
all_contacts() ->
    transaction(fun() ->
                mnesia:match_object(#db_contact{_='_'})
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
%%%
%% Helpers
%%%

save_attachment(_Type, _Id, [], _N) ->
    ok;
save_attachment(Type, Id, [File|Rest], N) ->
    mnesia:write(#db_attachment{id=N, file=File, type=Type, tid=Id}),
    save_attachment(Type, Id, Rest, N+1).
iterate(_, []) ->
    [];
iterate(Type, [Id|R]) ->
    mnesia:read(Type, Id) ++ iterate(Type, R).

iterate(_, [], _) ->
    [];
iterate(Type, [Id|R], Fun) ->
    Fun(Type, Id) ++ iterate(Type, R).

get_subgroup(G) ->
    Groups = mnesia:match_object(#db_group{subgroups=G, _='_'}),
    Sub = lists:map(fun(#db_group{id=N}=Gr) ->
                    Gr#db_group{subgroups=get_subgroup(N)}
        end, Groups).
             
