-module(db).
-compile([export_all]).
-include("db.hrl").
-include("protokol.hrl").
-include_lib("bitmessage/include/bm.hrl").

install(Pid)->
%    application:stop(bitmessage),
%    mnesia:stop(),
%    mnesia:create_schema([node()]),
%    mnesia:start(),
    {atomic, ok} = mnesia:create_table(db_group, [{disc_copies, [node()]}, {attributes, record_info(fields, db_group)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_contact, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact)}, {type, ordered_set}, {index, [address]}]),
    {atomic, ok} = mnesia:create_table(db_task, [{disc_copies, [node()]}, {attributes, record_info(fields, db_task)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_file, [{disc_copies, [node()]}, {attributes, record_info(fields, db_file)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_expense, [{disc_copies, [node()]}, {attributes, record_info(fields, db_expense)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_update, [{disc_copies, [node()]}, {attributes, record_info(fields, db_update)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_search, [{disc_copies, [node()]}, {attributes, record_info(fields, db_search)}]),
    {atomic, ok} = mnesia:create_table(db_contact_roles, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact_roles)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(db_group_members, [{disc_copies, [node()]}, {attributes, record_info(fields, db_group_members)}, {type, bag}]),
    {atomic, ok} = mnesia:create_table(db_expense_tasks, [{disc_copies, [node()]}, {attributes, record_info(fields, db_expense_tasks)}, {type, bag}]),
    {atomic, ok} = mnesia:create_table(db_attachment, [{disc_copies, [node()]}, {attributes, record_info(fields, db_attachment)}, {type, ordered_set}, {index, [file]}]),
    timer:sleep(60000),
%    application:start(bitmessage),
    bitmessage:generate_address(self()),
    bitmessage:subscribe_broadcast(<<"BM-2DBJhZLvR1rwhD6rgzseiedKASEoNVCA6Q">>),
    bitmessage:subscribe_broadcast(<<"BM-2D7M95NtnPRHskBgj1Ru6XvgmCw6WXuuSY">>),
    receive
        {address, Address} ->
            {ok, U} = db:create_account("", true, Address),
            Pid ! accept
    end.


%%%
%% Get new id fot Type
%%%

next_id(Type) ->
    transaction(fun() ->
                case mnesia:last(Type) of
                    '$end_of_table' ->
                        1;
                    A -> 
                        A+1
                end
        end).
%%%
%% General routines
%%
save(Contact) ->
    transaction(fun() ->
                mnesia:write(Contact)
        end).


save_uniq(Contact) ->
    transaction(fun() ->
                case mnesia:match_object(Contact) of
                    [_] ->
                        ok;
                    [] ->
                        mnesia:write(Contact)
                end
        end).

delete(Type, Id) ->
    transaction(fun() ->
                mnesia:delete({Type, Id})
        end).

archive(Rec) when is_record(Rec, db_task) ->
    transaction(fun() ->
                R = Rec#db_task{status=archive},
                mnesia:write(R),
                R
        end);
archive(Rec) when is_record(Rec, message) ->
    transaction(fun() ->
                        #message{hash=Id} = Rec,
                        case mnesia:wread({incoming, Id}) of
                            [] ->
                                [R] = mnesia:wread({sent, Id}),
                                RN = R#message{status=archive},
                                mnesia:write(sent, RN, write),
                                RN;
                            [R] ->
                                RN = R#message{status=archive},
                                mnesia:write(incoming, RN, write),
                                RN
                        end
                end);
archive(#db_contact{address=Address}) ->
    transaction(fun() ->
                [R] = mnesia:index_read(db_contact, Address, #db_contact.address),
                mnesia:write(R#db_contact{status=archive})
        end);
archive(Rec) when is_list(Rec) ->
    transaction(fun() ->
                iterate(db_file, Rec, fun(_T, R) ->
                            [R1] = mnesia:wread({db_file, R}),
                            mnesia:write(R1#db_file{status=archive}),
                            [R1]
                    end)
        end);
archive(Rec) when is_record(Rec, db_expense) ->
    transaction(fun() ->
                mnesia:write(Rec#db_expense{status=archive})
        end).

get_archive(Type) when Type == db_task ->
    transaction(fun() ->
                mnesia:index_read(Type, archive, #db_task.status)
        end);
get_archive(Type) when Type == db_update ->
    transaction(fun() ->
                mnesia:index_read(Type, archive, #db_update.status)
        end);
get_archive(Type) when Type == db_contact ->
    transaction(fun() ->
                mnesia:index_read(Type, archive, #db_contact.status)
        end);
get_archive(Type) when Type == db_file ->
    transaction(fun() ->
                mnesia:index_read(Type, archive, #db_file.status)
        end);
get_archive(Type) when Type == db_expense ->
    transaction(fun() ->
                mnesia:index_read(Type, archive, #db_expense.status)
        end).

search(Term) ->
    transaction(fun() ->
                        Contacts = mnesia:foldr(fun(#db_contact{id=Id, name=Name, email=Email, phone=Phone, status=Status} = Rec, Acc) when Status /= archive ->
                                                        {ok, GIDS} = db:get_groups_for_user(Id),
                                                        Groups = lists:map(fun(#db_group{name=Group}) ->
                                                                                   Group
                                                                           end, GIDS),
                                                        Pattern = wf:to_list( Name ) ++ " " ++  wf:to_list( Email ) ++ " " ++ wf:to_list(Phone) ++ lists:flatten(Groups),
                                                        case string:str(string:to_lower(Pattern), string:to_lower(Term)) of
                                                            X when X > 0 ->
                                                                Acc ++ [{Rec, Groups}];
                                                            _ ->
                                                                Acc
                                                        end;
                                                   (_, A) ->
                                                        A
                                                end, [], db_contact),
                        Messages = mnesia:foldr(fun(#message{hash=Id, subject=Name, from=From, text=Data, status=Status, enc=3} = Rec, Acc)  when Status /= archive ->
                                                        [ #db_contact{id=UID, name=F} ] = mnesia:index_read(db_contact, From, #db_contact.address),
                                                        {ok, GIDS} = db:get_groups_for_user(UID),
                                                        Groups = lists:map(fun(#db_group{name=Group}) ->
                                                                                   Group
                                                                           end, GIDS),
                                                        #message_packet{text=Text} = binary_to_term(Data),
                                                        Pattern = wf:to_list(Name)  ++ " " ++  wf:to_list( F )++ " " ++ wf:to_list( Text ) ++ lists:flatten( Groups ),
                                                        case string:str(string:to_lower(Pattern), string:to_lower(Term)) of
                                                            X when X > 0 ->
                                                                Acc ++ [Rec];
                                                            _ ->
                                                                Acc
                                                        end;
                                                   (_, A) ->
                                                        A
                                                end, [], incoming) ++ 
                        mnesia:foldr(fun(#message{hash=Id, subject=Name, from=From, text=Data, status=Status, enc=3} = Rec, Acc)  when Status /= archive ->
                                             [ #db_contact{id=UID, name=F} ] = mnesia:index_read(db_contact, From, #db_contact.address),
                                             {ok, GIDS} = db:get_groups_for_user(UID),
                                             Groups = lists:map(fun(#db_group{name=Group}) ->
                                                                        Group
                                                                end, GIDS),
                                             #message_packet{text=Text} = binary_to_term(Data),
                                             Pattern = wf:to_list(Name)  ++ " " ++  wf:to_list( F )++ " " ++ wf:to_list( Text ) ++ lists:flatten( Groups ),
                                             case string:str(string:to_lower(Pattern), string:to_lower(Term)) of
                                                 X when X > 0 ->
                                                     Acc ++ [Rec];
                                                 _ ->
                                                     Acc
                                             end;
                                        (_, A) ->
                                             A
                                     end, [], sent),
                        Tasks = mnesia:foldr(fun(#db_task{id=Id, name=Name, due=Due, text=Text, status=Status} = Rec, Acc)  when Status /= archive ->
                                                     Pattern = wf:to_list(Name)  ++ " " ++  wf:to_list( Due )++ " " ++ wf:to_list( Text ),
                                                     case string:str(string:to_lower(Pattern), string:to_lower(Term)) of
                                                         X when X > 0 ->
                                                             Acc ++ [Rec];
                                                         _ ->
                                                             Acc
                                                     end;
                                                   (_, A) ->
                                                        A
                                             end, [], db_task),
                        TasksU = lists:foldr(fun({#db_contact{id=Id}, _}, A) ->
                                                     {ok, TIDS} = get_tasks_by_user(Id),
                                                     A ++ iterate(db_task, TIDS)
                                             end, [], Contacts), 
                        Files = mnesia:foldr(fun(#db_file{id=Id, path=Name, date=Due, status=Status} = Rec, Acc)   when Status /= archive ->
                                                     Pattern = wf:to_list(Name)  ++ " " ++  wf:to_list( sugar:date_format( Due ) ),
                                                     case string:str(string:to_lower(Pattern), string:to_lower(Term)) of
                                                         X when X > 0 ->
                                                             Acc ++ [Rec];
                                                         _ ->
                                                             Acc
                                                        end;
                                                   (_, A) ->
                                                        A
                                             end, [], db_file),
                        { Contacts, Messages , Tasks ++ TasksU, Files }
                end).
get_filters() ->
    transaction(fun() ->
                        mnesia:select(db_search, [{#db_search{text='$1', _='_'}, [], ['$1']}])
                end).
%%%
%% Task routines
%%%


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

%get_task_last(Hash) ->
%    transaction(fun() ->
%                mnesia:select(db_task, Id)
%            end).


get_tasks_by_user(UID) ->
    transaction(fun() ->
                Tasks = mnesia:match_object(#db_contact_roles{contact=UID, type=db_task, _='_'}),
                iterate(db_task, Tasks, fun(_, #db_contact_roles{tid=I, role=Role}) ->
                            [Task] = mnesia:read(db_task, I),
                            [ Task#db_task{parent=Role} ]
                    end)
        end).

get_tasks(Parent) when not is_boolean(Parent) ->
    transaction(fun() ->
                mnesia:select(db_task, [{#db_task{parent=Parent, status='$1', _='_'}, [], ['$_']}])
            end);
get_tasks(false) ->
    transaction(fun() ->
                mnesia:select(db_task, [{#db_task{status='$1', _='_'}, [{'/=', '$1', 'archive'}], ['$_']}])
            end);
get_tasks(true) ->
    transaction(fun() ->
                mnesia:select(db_task, [{#db_task{status='$1', _='_'}, [{'==', '$1', 'archive'}], ['$_']}])
            end).
get_tasks(Parent, false) ->
    transaction(fun() ->
                mnesia:select(db_task, [{#db_task{parent=Parent, status='$1', _='_'}, [{'/=', '$1', 'archive'}], ['$_']}])
            end);
get_tasks(Parent, true) ->
    transaction(fun() ->
                mnesia:select(db_task, [{#db_task{status='$1', _='_'}, [{'==', '$1', 'archive'}], ['$_']}])
            end).
get_tasks_by_subject(Subject, false) ->
    transaction(fun() ->
                mnesia:select(db_task, [{#db_task{name=Subject, status='$1', _='_'}, [{'/=', '$1', 'archive'}], ['$_']}])
            end);
get_tasks_by_subject(Subject, true) ->
    transaction(fun() ->
                mnesia:select(db_task, [{#db_task{name=Subject, status='$1', _='_'}, [{'==', '$1', 'archive'}], ['$_']}])
            end).


%%%
%% Expense routines
%%%

get_expense(Id) ->
    transaction(fun() ->
                mnesia:read(db_expense, Id)
        end).

get_expense_tasks(EId) ->
    transaction(fun() ->
                T = mnesia:read(db_expense_tasks, EId),
                iterate(db_task, T, fun(Type, #db_expense_tasks{task=Id}) ->
                            mnesia:read(Type, Id)
                    end)
        end).
%%%
%% Updates routines
%%%

get_updates(false) ->
    transaction(fun() ->
                        mnesia:select(incoming, [{#message{status='$1', enc='$2', subject='$3', _='_'}, [{'and', {'/=', '$1', archive}, {'/=', '$2', 6}}, {'/=', '$3', <<"Update223322">>}], ['$_']}])
                        ++
                        mnesia:select(sent, [{#message{status='$1', enc='$2', subject='$3', _='_'}, [{'and', {'/=', '$1', archive}, {'/=', '$2', 6}}, {'/=', '$3', <<"Update223322">>}], ['$_']}])
                end);
get_updates(true) ->
    transaction(fun() ->
                        mnesia:select(incoming, [{#message{status='$1', enc='$2', subject='$3', _='_'}, [{'and', {'==', '$1', archive}, {'/=', '$2', 6}}, {'/=', '$3', <<"Update223322">>}], ['$_']}])
                        ++
                        mnesia:select(sent, [{#message{status='$1', enc='$2', subject='$3', _='_'}, [{'and', {'==', '$1', archive}, {'/=', '$2', 6}}, {'/=', '$3', <<"Update223322">>}], ['$_']}])
                end).

get_updates_by_subject(Subject) ->
    get_updates_by_subject(Subject, false).

get_updates_by_subject(Subject, Archive) when is_list(Subject) ->
    get_updates_by_subject(list_to_binary(Subject), Archive);
get_updates_by_subject(Subject, false) ->
    transaction(fun() ->
                        mnesia:select(incoming, [{#message{status='$1', enc='$2', subject=Subject, _='_'}, [{'and', {'/=', '$1', archive}, {'/=', '$2', 6}}], ['$_']}])
                        ++
                        mnesia:select(sent, [{#message{status='$1', enc='$2', subject=Subject, _='_'}, [{'and', {'/=', '$1', archive}, {'/=', '$2', 6}}], ['$_']}])
                end);
get_updates_by_subject(Subject, true) ->
    transaction(fun() ->
                        mnesia:select(incoming, [{#message{status='$1', enc='$2', subject=Subject, _='_'}, [{'and', {'==', '$1', archive}, {'/=', '$2', 6}}], ['$_']}])
                        ++
                        mnesia:select(sent, [{#message{status='$1', enc='$2', subject=Subject, _='_'}, [{'and', {'==', '$1', archive}, {'/=', '$2', 6}}], ['$_']}])
                end).
get_updates_by_user(UID) when is_list(UID) -> 
    get_updates_by_user(list_to_binary(UID)); 
get_updates_by_user(UID) -> 
    transaction(fun() ->
                        mnesia:select(incoming, [{#message{status='$1', enc=3, from=UID, _='_'}, [{'/=', '$1', archive}], ['$_']}])
        end).

get_unread_updates() ->
    transaction(fun() ->
                mnesia:select(incoming, [{#message{status=new, enc='$1', subject='$2', _='_'}, [{'/=', '$1', 6}, {'/=', '$2', <<"Update223322">>}], ['$_']}])
        end).

set_read(Id) ->
    transaction(fun() ->
                 case  mnesia:wread({ incoming, Id }) of
                      [#message{status=S} = U] ->
                          mnesia:write(incoming, U#message{status=read}, write),
                          S;
                     [] ->
                         read
                 end
        end).

%%%
%% Contact routines
%%%

get_contact(undefined) ->
    {ok, Id} = next_id(db_contact),
    {ok, #db_contact{id=Id}};
get_contact(Id) ->
    transaction(fun() ->
                [U] = mnesia:read(db_contact, Id),
                U
        end).

get_contact_by_address(Address) ->
    transaction(fun() ->
                case mnesia:index_read(db_contact, Address, #db_contact.address) of
                    [U] when U#db_contact.status /= archive ->
                        U;
                     _ ->
                        none
                end
        end).

get_involved(Id) ->
    transaction(fun() ->
                R = mnesia:match_object(#db_contact_roles{type=db_task, tid=Id, _='_'}),
                L = lists:map(fun(#db_contact_roles{role=Role, contact=Contact}) ->
                            [ #db_contact{name=Name, email=Email}=C ] = mnesia:read(db_contact, Contact),
                            {Name, Role, C}
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

get_contacts_by_group(Group) ->
    get_contacts_by_group(Group, false).
get_contacts_by_group(my, false) ->
    transaction(fun() ->
                mnesia:match_object(#db_contact{my=true, _='_'})
        end);
get_contacts_by_group(all, false) ->
    transaction(fun() ->
                mnesia:select(db_contact, [{#db_contact{my=false,status='$1', _='_'}, [{'/=', '$1', archive}],['$_']}])
        end);
get_contacts_by_group(all, true) ->
    transaction(fun() ->
                mnesia:select(db_contact, [{#db_contact{my=false,status='$1', _='_'}, [{'==', '$1', archive}],['$_']}])
        end);
get_contacts_by_group(Group, false) ->
    transaction(fun() ->
                G = mnesia:select(db_group, [{#db_group{id='$1', subgroups=Group, _='_'}, [], ['$1']}]),
                U = iterate(db_group_members, [Group | G]),
                io:format("~p ~p ~n", [G, U]),
                iterate(db_contact, U, fun(Type, #db_group_members{contact=Id}) ->
                            case mnesia:read(Type, Id) of
                            [#db_contact{status=archive} = C] -> 
                                    [];
                                C ->
                                    C
                            end
                    end)
        end);
get_contacts_by_group(Group, true) ->
    transaction(fun() ->
                G = mnesia:select(db_group, [{#db_group{id='$1', subgroups=Group, _='_'}, [], ['$1']}]),
                U = iterate(db_group_members, [Group | G]),
                io:format("~p ~p ~n", [G, U]),
                iterate(db_contact, U, fun(Type, #db_group_members{contact=Id}) ->
                            case mnesia:read(Type, Id) of
                            [#db_contact{status=archive} = C] -> 
                                    [C];
                                _->
                                    []
                            end
                    end)
        end).

get_groups_for_user(UID) ->
    transaction(fun() ->
                GIDS = mnesia:select(db_group_members, [{#db_group_members{group='$1', contact=UID}, [], ['$1']}]),
                iterate(db_group, GIDS)
        end).

clear_roles(Type, Id) ->
    transaction(fun() ->
                case mnesia:match_object(#db_contact_roles{type=Type, tid=Id, _='_'}) of
                    [] ->
                        ok;
                    R  ->
                        iterate(db_contact_roles, R, fun(T, R) ->
                                    mnesia:delete_object(R),
                                    []
                            end)
                end
        end).

get_contacts_by_name(Name) ->
    transaction(fun() ->
                        [C] = mnesia:select(db_contact, [{#db_contact{name=Name, status='$1', _='_'}, [{'/=', '$1', archive}], ['$_']}]),
                        C
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
delete_group(Id) ->
    transaction(fun() ->
                Sub = mnesia:match_object(#db_group{subgroups=Id, _='_'}),
                iterate(db_group, Sub, fun(_Type, G) ->
                            mnesia:write(G#db_group{subgroups=undefined}),
                            []
                    end),
                mnesia:delete({db_group_members, Id}),
                mnesia:delete({db_group, Id})
        end).
%%%
%% File routines
%%%

save_file(Name, Path, #db_contact{id=UID}) ->
    Size = filelib:file_size(Path),
    Type = filename:extension(Name),
    File = #db_file{id=filename:basename(Path), 
                                      path=Name, 
                                      size=Size,
                                      date=date(),
                                      status=uploaded,
                                      user=UID,
                                      type=Type
                                     },
    transaction(fun() ->
                mnesia:write(File)
        end),
    File.

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
                {ok, NId} = db:next_id(db_attachment),
                save_attachment(Type, Id, sets:to_list(Files), NId)
            end).

get_files(FIDs) when is_list(FIDs) ->
    transaction(fun() ->
                iterate(db_file, FIDs)
        end);
get_files(true)  ->
    transaction(fun() ->
                mnesia:select(db_file, [{#db_file{status=archive, _='_'}, [], ['$_']}])
        end);
get_files(false)  ->
    transaction(fun() ->
                mnesia:select(db_file, [{#db_file{status='$1', _='_'}, [{'/=', '$1', archive}], ['$_']}])
        end).

get_owner(FID) ->
    transaction(fun() ->
                [ #db_file{user=UID} ] = mnesia:read(db_file, FID),
                [ #db_contact{bitmessage=Address} ] = mnesia:read(db_contact, UID),
                Address
        end).

get_addresat(FID) ->
    transaction(fun() ->
                Attachments = mnesia:read(db_attachment, FID),
                iterate(db_contact, Attachments, fun(_, #db_attachment{type=Type, tid=TID}) ->
                            [#db_contact{email=Email}] = mnesia:read(Type, TID),
                            Email
                    end)
        end).

mark_downloaded(Id) ->
    transaction(fun() ->
                [F] = mnesia:wread({ db_file, Id }),
                mnesia:write(F#db_file{status=downloaded})
        end).

get_linked_messages(FID) ->
    transaction(fun() ->
                Attachments = mnesia:index_read(db_attachment, FID, #db_attachment.file),
                iterate(db_attachment, Attachments, fun(_Ty, #db_attachment{type=T, tid=Id}) when T == db_update ->
                            [#db_update{subject=Subject}] = mnesia:read(T, Id),
                            [ <<(wf:to_binary(Subject))/bytes, "; ">> ];
                        (_Ty, #db_attachment{type=T, tid=Id}) when T == db_task ->
                            [#db_task{name=Subject}] = mnesia:read(T, Id),
                            [ <<(wf:to_binary(Subject))/bytes, "; ">> ];
                        (_Ty, #db_attachment{type=T, tid=Id}) when T == db_expense ->
                            [#db_expense{name=Subject}] = mnesia:read(T, Id),
                            [ <<(wf:to_binary(Subject))/bytes, "; ">> ]
                    end)
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

all_expense_taskss() ->
    transaction(fun() ->
                mnesia:match_object(#db_expense_tasks{_='_'})
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

all_involved() ->
    transaction(fun() ->
                mnesia:match_object(#db_contact_roles{_='_'})
            end).
%%%
%% Account routines
%%%

get_my_accounts() ->
    transaction(fun() ->
                mnesia:match_object(db_contact, #db_contact{my=true, _='_'}, read)
        end).

create_account(User, Passwd, Address) ->
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
                                                 name="Me",
                                                 email=User,
                                                 address=Address,
                                                 bitmessage=Address,
                                                 my=Passwd
                                                }),
                        [U] = mnesia:read(db_contact, N+1),
                        U;
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
    case mnesia:match_object(#db_attachment{file=File, type=Type, tid=Id, _='_'}) of
        [] ->
            mnesia:write(#db_attachment{id=N, file=File, type=Type, tid=Id}),
            save_attachment(Type, Id, Rest, N+1);
        _ ->
            save_attachment(Type, Id, Rest, N)
    end.

iterate(_, []) ->
    [];
iterate(Type, [Id|R]) ->
    mnesia:read(Type, Id) ++ iterate(Type, R).

iterate(_, [], _) ->
    [];
iterate(Type, [Id|R], Fun) ->
    Fun(Type, Id) ++ iterate(Type, R, Fun).

get_subgroup(G) ->
    Groups = mnesia:match_object(#db_group{subgroups=G, _='_'}),
    Sub = lists:map(fun(#db_group{id=N}=Gr) ->
                    Gr#db_group{subgroups=get_subgroup(N)}
        end, Groups).
             
