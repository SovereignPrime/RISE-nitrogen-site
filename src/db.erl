-module(db).
-compile([export_all]).
-include("db.hrl").
-include("protokol.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Simple shortcut to verify either the mnesia table  was properly set up or
%% already exists
-define(V(Response), verify_create_table(Response)).

install(Pid)->  % {{{1
    ?V(mnesia:create_table(db_group, [{disc_copies, [node()]}, {attributes, record_info(fields, db_group)}, {type, ordered_set}, {index, [name]}])),
    ?V(mnesia:create_table(db_contact, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact)}, {type, ordered_set}, {index, [address]}])),
    ?V(mnesia:create_table(db_task, [{disc_copies, [node()]}, {attributes, record_info(fields, db_task)}, {type, ordered_set}])),
    ?V(mnesia:create_table(db_update, [{disc_copies, [node()]}, {attributes, record_info(fields, db_update)}, {type, ordered_set}])),
    ?V(mnesia:create_table(db_file, [{disc_copies, [node()]}, {attributes, record_info(fields, db_file)}, {type, ordered_set}])),
    ?V(mnesia:create_table(db_expense, [{disc_copies, [node()]}, {attributes, record_info(fields, db_expense)}, {type, ordered_set}])),
    ?V(mnesia:create_table(db_search, [{disc_copies, [node()]}, {attributes, record_info(fields, db_search)}])),
    ?V(mnesia:create_table(db_contact_roles, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact_roles)}, {type, ordered_set}])),
    ?V(mnesia:create_table(db_group_members, [{disc_copies, [node()]}, {attributes, record_info(fields, db_group_members)}, {type, bag}])),
    ?V(mnesia:create_table(db_expense_tasks, [{disc_copies, [node()]}, {attributes, record_info(fields, db_expense_tasks)}, {type, bag}])),
    ?V(mnesia:create_table(db_attachment, [{disc_copies, [node()]}, {attributes, record_info(fields, db_attachment)}, {type, ordered_set}, {index, [file]}])),
    ?V(mnesia:create_table(db_task_tree, [{disc_copies, [node()]}, {attributes, record_info(fields, db_task_tree)}, {type, bag}, {index, [parent, visible]}])),
    ?V(mnesia:create_table(db_contact_note, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact_note)}, {type, ordered_set}, {index, [contact]}])),
    timer:sleep(60000),
    receiver:register_receiver(Pid),
    bitmessage:generate_address(),
    bitmessage:subscribe_broadcast(<<"BM-2DBJhZLvR1rwhD6rgzseiedKASEoNVCA6Q">>),
    bitmessage:subscribe_broadcast(<<"BM-2D7M95NtnPRHskBgj1Ru6XvgmCw6WXuuSY">>).

account(Pid, Address) ->  % {{{1
            {ok, U} = db:create_account("", true, Address),
            Pid ! accepted.

update() ->  % {{{1
	LastUpdate = 6,
	[update(N) || N <- lists:seq(1,LastUpdate)].

update(1) -> % {{{1
    Fields = mnesia:table_info(db_task_tree, attributes),
	Transform = fun({db_task_tree, T, P, V}) ->
		#db_task_tree{task=T,
				   	  parent=P,
				      time=bm_types:timestamp(),
				      visible=V}
	end,
    case Fields of
        [task, parent,visible] ->
            mnesia:transform_table(db_task_tree, Transform, record_info(fields, db_task_tree));
        _ ->
            ok
    end;

update(2) -> % {{{1
	Fields = mnesia:table_info(db_task, attributes),
	io:format("Fields: ~p",[Fields]),
	Transform = fun({db_task, ID, Due, Name, Text, Parent, Status}) ->
		io:format("Transforming Record~n"),
		#db_task{id=ID, due=Due, name=Name, text=Text, parent=Parent, status=Status, changes=[]}
	end,
	case Fields of
		[id, due, name, text, parent, status] ->
			mnesia:transform_table(db_task, Transform, record_info(fields, db_task));
		_ ->
			ok
	end;
update(3) ->  % {{{1
	Fields = mnesia:table_info(db_search, attributes),
    NFields = record_info(fields, db_search),
    mnesia:transform_table(db_search,
                           fun({db_search, Text, undefined}) ->
                                   #db_search{text=Text, name="Updated"};
                              (R) ->
                                   R
                           end,
                           NFields),
    mnesia:add_table_index(db_group, name);
update(4) ->  % {{{1
    NFields = record_info(fields, pubkey),
    mnesia:transform_table(pubkey,
                           fun(In) when size(In) == 7 ->
                                   LIn = tuple_to_list(In),
                                   LOut = LIn ++ [1000, 1000],
                                   list_to_tuple(LOut);
                              (R) ->
                                   R
                           end,
                           NFields);
update(5) ->  % {{{1
    mnesia:transaction(fun() ->
                        Files = mnesia:select(db_file, [{#db_file{status='$1', _='_'}, [{'/=', '$1', archive}], ['$_']}]),
                        lists:foreach(fun(#db_file{id=Id,
                                                   path=Path,
                                                   status=Status,
                                                   date=Date,
                                                   size=Size}) ->
                                              mnesia:write(#bm_file{
                                                              hash=wf:to_binary(Id),
                                                              name=Path,
                                                              size=Size,
                                                              time={Date, {0,0,0}},
                                                              status=Status})
                                      end,
                                      Files)
                       end);
update(6) ->  % {{{1
    mnesia:delete_table(db_update),
    ?V(mnesia:create_table(db_contact_note, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact_note)}, {type, ordered_set}, {index, [contact]}])),
    ?V(mnesia:create_table(db_update, [{disc_copies, [node()]}, {attributes, record_info(fields, db_update)}, {type, ordered_set}])).



qlc_result(QH) ->  % {{{1
    transaction(fun() ->
                        qlc:e(QH)
                end).

%%%
%% Get new id fot Type
%%%

next_id(Type) ->  % {{{1
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
save(Contact) ->  % {{{1
    transaction(fun() ->
                mnesia:write(Contact)
        end).

save_uniq(Contact) ->  % {{{1
    transaction(fun() ->
                case mnesia:match_object(Contact) of
                    [_] ->
                        ok;
                    [] ->
                        mnesia:write(Contact)
                end
        end).

delete(Type, Id) ->  % {{{1
    transaction(fun() ->
                mnesia:delete({Type, Id})
        end).


archive(Rec) when is_record(Rec, db_task) ->  % {{{1
    transaction(fun() ->
                R = Rec#db_task{status=archive},
                mnesia:write(R),
                R
        end);
archive(Rec) when is_record(Rec, message) ->  % {{{1
    transaction(fun() ->
                        #message{hash=Id} = Rec,
                         [R]  = mnesia:wread({message, Id}),
                         RN = R#message{status=archive},
                         mnesia:write(message, RN, write),
                         RN
                end);
archive(#db_contact{address=Address}) ->  % {{{1
    transaction(fun() ->
                [R] = mnesia:index_read(db_contact, Address, #db_contact.address),
                mnesia:write(R#db_contact{status=archive}),
                Updates = mnesia:select(message,
                                        [{#message{status='$1',
                                                   folder=incoming,
                                                   from=Address,
                                                   _='_'},
                                          [{'/=', '$1', archive}],
                                          ['$_']}]),
                lists:foreach(fun(M) ->
                                      db:save(M#message{status=archive})
                              end,
                              Updates)
                
        end);
archive(Rec) when is_list(Rec) ->  % {{{1
    transaction(fun() ->
                iterate(bm_file, Rec, fun(_T, R) ->
                            [R1] = mnesia:wread({bm_file, R}),
                            mnesia:write(R1#bm_file{status=archive}),
                            [R1]
                    end)
        end);
archive(Rec) when is_record(Rec, db_expense) ->  % {{{1
    transaction(fun() ->
                mnesia:write(Rec#db_expense{status=archive})
        end).

get_archive(Type) when Type == db_task ->  % {{{1
    transaction(fun() ->
                mnesia:index_read(Type, archive, #db_task.status)
        end);
get_archive(Type) when Type == db_update ->  % {{{1
    transaction(fun() ->
                mnesia:index_read(Type, archive, #db_update.status)
        end);
get_archive(Type) when Type == db_contact ->  % {{{1
    transaction(fun() ->
                mnesia:index_read(Type, archive, #db_contact.status)
        end);
get_archive(Type) when Type == db_file ->  % {{{1
    transaction(fun() ->
                mnesia:index_read(Type, archive, #bm_file.status)
        end);
get_archive(Type) when Type == db_expense ->  % {{{1
    transaction(fun() ->
                mnesia:index_read(Type, archive, #db_expense.status)
        end).

search_dates({0, M, D}=Date) ->  % {{{1
    MS = wf:to_list(M),
    DS = wf:to_list(D),
    get_by_date({'or', {'==', '$2', M}, {'==', '$3', D}},
                "\\d{4}-" ++ MS ++ "-\\d{2}|\\d{4}-\\d{2}-" ++ DS,
                Date);
search_dates({Y, 0, 0}=Date) ->  % {{{1
    YS = wf:to_list(Y),
    get_by_date({'==', '$3', Y}, 
                YS ++ "-\\d{2}-\\d{2}",
                Date);
search_dates({Y, M, D}=Date) when M > 12; D > 31 ->  % {{{1
    [];
search_dates({Y, M, D}=Date) ->  % {{{1
    get_by_date([{'==', '$1', Y}, {'==', '$2', M}, {'==', '$3', D}], 
                sugar:date_format(Date),
                Date).

search_groups(Term) ->  % {{{1
    transaction(fun() ->
                        Tab = mnesia:table(db_group),
                        QH = qlc:q([G || G <- Tab, 
                                         re:run(G#db_group.name, Term, [caseless]) /= nomatch]),
                        qlc:e(QH)
                end).

search_contacts(Term) ->  % {{{1
    transaction(fun() ->
                        Tab = mnesia:table(db_contact),
                        QH = qlc:q([G || G <- Tab, 
                                         G#db_contact.status /= archive,
                                         re:run(G#db_contact.name ++ G#db_contact.email, Term, [caseless]) /= nomatch]),
                        qlc:e(QH)
                end).

search_files(Terms) ->  % {{{1
    check_roles(Terms, fun() ->
                               Term = search:get_term(Terms),
                               transaction(fun() ->
                                                   UReq = case {dict:find("Group", Terms), dict:find("Contact", Terms)} of
                                                              {error, error} ->
                                                                  [];
                                                              {{ok, G}, _} ->
                                                                  [#db_group{id=GID}] = mnesia:index_read(db_group, G, #db_group.name),
                                                                  case lists:map(fun(#db_group_members{contact=UID}) ->
                                                                                         {'==', '$1', UID}
                                                                                 end,
                                                                                 mnesia:read(db_group_members,
                                                                                             GID)) of
                                                                      [] ->
                                                                          [];
                                                                      L ->
                                                                          [list_to_tuple(['orelse' | L])]
                                                                  end;
                                                              {error, {ok, U}} ->
                                                                  {ok, #db_contact{id=UID}} = get_contacts_by_name(U),
                                                                  [{'==', '$1', UID}]
                                                          end,
                                                   DReq = case {dict:find("Daterange", Terms), dict:find("Date", Terms)} of
                                                              {error, error} ->
                                                                  [];
                                                              {{ok, D}, _} when is_list(D) ->
                                                                  [SD, ED] = string:tokens(D, " "),
                                                                  [{'>=', '$2', {const, sugar:date_from_string(SD)}}, {'<', '$2', {const, sugar:date_from_string(ED)}}];
                                                              {{ok, {SD, ED}}, _} ->
                                                                  [{'>=', '$2', {const, SD}}, {'<', '$2', {const, ED}}];
                                                              {error, {ok, D}} ->
                                                                  [{'==', '$2', {const, D}}]
                                                          end,
                                                   Request = UReq ++ DReq,

                                                   Tab = mnesia:table(db_file, [{traverse, {select, [{#db_file{user='$1', date='$2', _='_'}, Request, ['$_']}]}}]),
                                                   QH = qlc:q([G || G <- Tab,
                                                                    G#db_file.status /= archive,
                                                                    re:run(G#db_file.path, Term, [caseless]) /= nomatch]),
                                                   qlc:e(QH)
                                           end)
                       end).

search_messages(Terms) ->  % {{{1
    check_roles(Terms, fun() ->
                        Term = search:get_term(Terms),
                        transaction(fun() ->
                                            UReq = case {dict:find("Group", Terms), dict:find("Contact", Terms)} of
                                                       {error, error} ->
                                                           [];
                                                       {{ok, G}, _} ->
                                                           [#db_group{id=GID}] = mnesia:index_read(db_group, G, #db_group.name),
                                                           case  lists:map(fun(#db_group_members{contact=UID}) ->
                                                                                   [#db_contact{address=A}] = mnesia:read(db_contact, UID),
                                                                                   {'orelse', {'==', '$1', A}, {'==', '$2', A}}
                                                                           end,
                                                                           mnesia:read(db_group_members, GID)) of
                                                               [] ->
                                                                   [];
                                                               L ->
                                                                   [list_to_tuple(['orelse' | L])]
                                                           end;
                                                       {error, {ok, U}} ->
                                                           {ok, #db_contact{address=A}} = get_contacts_by_name(U),
                                                           [{'orelse', {'==', '$2', A}, {'==', '$1', A}}]
                                                   end,
                                            Msg = mnesia:table(message,
                                                               [{traverse,
                                                                 {select,
                                                                  [{#message{from='$1',
                                                                             to='$2',
                                                                             _='_'},
                                                                    UReq,
                                                                    ['$_']}]}}]),
                                            QH = qlc:q([G || G <- Msg,
                                                             G#message.status /= archive,
                                                             G#message.enc == 3,
                                                             try
                                                                 #message_packet{time=TS,
                                                                                 text=Text} = binary_to_term(G#message.text),
                                                                 CompDateRange = fun(SD1, ED1) ->
                                                                                         {DI, _} = sugar:timestamp_to_datetime(TS),
                                                                                         DI >= sugar:date_from_string(SD1) andalso DI < sugar:date_from_string(ED1)
                                                                                 end,
                                                                 case {dict:find("Daterange",
                                                                                 Terms),
                                                                       dict:find("Date",
                                                                                 Terms)} of
                                                                     {error,
                                                                      error} ->
                                                                         true;
                                                                     {{ok, {SD1, ED1}}, _} ->
                                                                         CompDateRange(SD1, ED1);
                                                                     {{ok, D}, _} when is_list(D) ->
                                                                         [SD1, ED1] = string:tokens(D, " "),
                                                                         CompDateRange(SD1, ED1);
                                                                     {error, {ok, D1}} ->
                                                                         {DI, _} = sugar:timestamp_to_datetime(TS),
                                                                         DI == sugar:date_from_string(D1)
                                                                 end andalso
                                                                 re:run(wf:to_list(G#message.subject) ++ wf:to_list(Text), Term, [caseless]) /= nomatch
                                                             catch 
                                                                 error:badarg ->
                                                                     false
                                                             end]),
                                            qlc:e(QH)
                                    end)
                       end).

search_tasks(Terms) ->  % {{{1
    Term = search:get_term(Terms),
    transaction(fun() ->
                        Tasks = lists:foldl(fun({T, _}, A) when T=="Term";T == "Date"; T == "Daterange" ->
                                                    A;
                                               ({T, U}, A) when T == "Responsible"; T == "Accountable"; T == "Informed"; T =="Consulted" ->
                                                    {ok, #db_contact{id=UID}} = get_contacts_by_name(U),
                                                    {ok, Ts} = get_tasks_by_user(UID, string:to_lower(T)) ,
                                                    lists:usort(A ++ Ts);
                                               ({"Group", G}, A) ->
                                                    [#db_group{id=GID}] = mnesia:index_read(db_group, G, #db_group.name),
                                                    lists:usort(lists:map(fun(#db_group_members{contact=UID}) ->
                                                                                  {ok, Ts} = get_tasks_by_user(UID) 
                                                                          end, mnesia:read(db_group_members, GID)));
                                               ({"Contact", U}, A) ->
                                                    {ok, #db_contact{id=UID}} = get_contacts_by_name(U),
                                                    {ok, Ts} = get_tasks_by_user(UID) ,
                                                    Ts
                                            end, [], dict:to_list(Terms)),

                        QH = qlc:q([G || G <- Tasks, 
                                         G#db_task.status /= archive,
                                         case {dict:find("Daterange", Terms), dict:find("Date", Terms)} of
                                             {error, error} ->
                                                 true;
                                             {{ok, D}, _} when is_list(D) ->
                                                 [SD1, ED1] = string:tokens(D, " "),
                                                 DI = sugar:date_from_string(G#db_task.due),
                                                 DI /= "" andalso DI >= sugar:date_from_string(SD1) andalso DI < sugar:date_from_string(ED1);
                                             {{ok, {SD1, ED1}}, _} ->
                                                 DI = sugar:date_from_string(G#db_task.due),
                                                 DI /= "" andalso DI >= sugar:date_from_string(SD1) andalso DI < sugar:date_from_string(ED1);
                                             {error, {ok, D1}} ->
                                                 DI = sugar:date_from_string(G#db_task.due),
                                                 DI /= "" andalso DI == sugar:date_from_string(D1)
                                         end,
                                         re:run(wf:to_list(G#db_task.name) ++ wf:to_list(G#db_task.text), Term, [caseless]) /= nomatch]),
                        qlc:e(QH)
                end).

get_filters() ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_search, [{#db_search{_='_'}, [], ['$_']}])
                end).
%%%%
%%% Task routines
%%%%
%
%
save_subtask(Id, PId, Time) ->  % {{{1
    transaction(fun() ->
                        Tree = mnesia:select(db_task_tree, [{#db_task_tree{task=Id, time='$1', _='_'}, [{'>', '$1', Time}], ['$_']}]),
                        case Tree of
                            [] ->
                                [ C ] = mnesia:wread({ db_task, Id }),
                                mnesia:write(C#db_task{parent=PId});
                            _ ->
                                ok
                        end,
                        mnesia:write(#db_task_tree{task=Id, parent=PId, time=Time})
                end).

get_task() ->  % {{{1
    transaction(fun() ->
                        mnesia:read(db_task, mnesia:last(db_task))
                end).

get_task(Id) ->   % {{{1
    transaction(fun() ->
                        mnesia:read(db_task, Id)
                end).

get_task_history(Hash) ->  % {{{1
    transaction(fun() ->
                        mnesia:foldr(fun(#message{hash=Id, enc=4, text=D, status=S}=Msg, A) when S /= archive ->
                                             case binary_to_term(D) of
                                                 #task_packet{id=Hash}=Task ->
                                                     A ++ [Msg];
                                                 _ ->
                                                     A
                                             end;
                                        (_, A) ->
                                             A
                                     end, [], message)
                end).


get_tasks_by_user(UID) ->  % {{{1
    get_tasks_by_user(UID, '_').
get_tasks_by_user(UID, Role) ->  % {{{1
    transaction(fun() ->
                        Tasks = mnesia:match_object(#db_contact_roles{contact=UID, type=db_task, role=Role, _='_'}),
                        iterate(db_task, Tasks, fun(_, #db_contact_roles{tid=I, role=Role}) ->
                                                        case mnesia:read(db_task, I) of
                                                            [Task] ->
                                                                [ Task#db_task{parent=Role} ];
                                                            [] ->
                                                                []
                                                        end
                                                end)
                end).

archive_op(true) -> '==';  % {{{1
archive_op(false) -> '/='.  % {{{1

get_tasks(Parent) when not is_boolean(Parent) ->  % {{{1
    get_tasks(Parent, false);
get_tasks(Archive) ->  % {{{1
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{status='$1', _='_'}, [{ArchOp, '$1', 'archive'}], ['$_']}])
                end).

get_tasks(Parent, Archive) ->  % {{{1
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{parent=Parent, status='$1', _='_'}, [{ArchOp, '$1', 'archive'}], ['$_']}])
                end).

get_tasks_due_today(Archive) ->
    Today = sugar:date_format(date()),
    error_logger:info_msg("searching: ~p",[Today]),
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{status='$1', due=Today, _='_'}, [{ArchOp, '$1', 'archive'}], ['$_']}])
                end).

get_tasks_no_deadline(Archive) ->
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{status='$1', due="", _='_'}, [{ArchOp, '$1', 'archive'}], ['$_']}])
                end).

get_tasks_completed(_Archive) ->
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{status=complete,  _='_'}, [], ['$_']}])
                end).

are_all_child_tasks_complete(Taskid) ->
    {ok, Tasks} = get_tasks(Taskid, false),
    lists:all(fun(Task) ->
                      Task#db_task.status=:=complete andalso
                      are_all_child_tasks_complete(Task#db_task.id)
              end, Tasks).

get_orphan_tasks(Archive) ->
    {ok, Tasks} = get_tasks(Archive),
    Taskids = [T#db_task.id || T <- Tasks],
    Orphans = [T || T <- Tasks,
                    T#db_task.parent=/=undefined,
                    not(lists:member(T#db_task.parent, Taskids))],
    {ok, Orphans}.

get_tasks_by_subject(Subject, false) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{name=Subject, status='$1', _='_'}, [{'/=', '$1', 'archive'}], ['$_']}])
                end);
get_tasks_by_subject(Subject, true) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{name=Subject, status='$1', _='_'}, [{'==', '$1', 'archive'}], ['$_']}])
                end).
save_task_tree(Id, Parent) ->  % {{{1
    transaction(fun() ->
                        TT = #db_task_tree{task=Id, parent=Parent},
                        case mnesia:read(db_task, Id) of
                            [] ->
                                mnesia:write(TT#db_task_tree{visible=false});
                            [_] ->
                                mnesia:write(TT#db_task_tree{visible=true})
                        end
                end).

search_parent(Id, PId) ->  % {{{1
    transaction(fun() ->
                        search_parent_rec(Id, PId)
                end).

get_children(UID, Time) ->  % {{{1
    transaction(fun() ->
                        CS = mnesia:select(db_task_tree, [{#db_task_tree{parent=UID,time='$1', _='_'}, [{'>', '$1', Time}], ['$_']}]),
                        CSS = lists:sort(fun(#db_task_tree{time=A}, #db_task_tree{time=B}) -> A > B end, CS),
                        lists:foldl(fun(#db_task_tree{task=T}, A) ->
                                            case lists:keymember(T, 2, A) of
                                                true ->
                                                    A;
                                                false ->
                                                    [T|A]
                                            end
                                    end, [], CSS)
                end). 

task_status_list() ->  % {{{1
    [{new, "New"},
     {accepted, "Accepted"},
     {in_progress, "In Progress"},
     {complete, "Complete"},
     {archive, "Archived"}].

nice_task_status_name(changed) ->  % {{{1
    "Changed";
nice_task_status_name(Status) ->  % {{{1
    proplists:get_value(Status, task_status_list()).

sanitize_task_status(Status) when is_list(Status) ->  % {{{1
    try 
        S = list_to_existing_atom(Status),
        {S, _} = lists:keyfind(S, 1, task_status_list()),
        S
    catch
        Class:Error ->
            error_logger:warning_msg("Invalid task status: ~p~nStacktrace: ~p",[Status, erlang:get_stacktrace()]),
            new %% if task status is a fail, generate error message and use new
    end.



%%%
%% Expense routines
%%%

get_expense(Id) ->  % {{{1
    transaction(fun() ->
                        mnesia:read(db_expense, Id)
                end).

get_expense_tasks(EId) ->  % {{{1
    transaction(fun() ->
                        T = mnesia:read(db_expense_tasks, EId),
                        iterate(db_task, T, fun(Type, #db_expense_tasks{task=Id}) ->
                                                    mnesia:read(Type, Id)
                                            end)
                end).
%%%
%% Updates routines
%%%
get_update(Id) ->   % {{{1
    transaction(fun() ->
                        [U] = mnesia:read(message, Id),
                        U
                end).
get_updates(false) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(message, [{#message{status='$1',
                                                       enc='$2',
                                                       subject='$3',
                                                       _='_'},
                                              [{'and',
                                                {'/=', '$1', archive},
                                                {'/=', '$2', 6}},
                                               {'/=', '$3', <<"Update223322">>}],
                                              ['$_']}])
                end);
get_updates(true) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(message,
                                      [{#message{status='$1',
                                                 enc='$2',
                                                 subject='$3',
                                                 _='_'},
                                        [{'and',
                                          {'==', '$1', archive},
                                          {'/=', '$2', 6}},
                                         {'/=', '$3', <<"Update223322">>}],
                                        ['$_']}])
                end).

get_updates_by_subject(Subject) ->  % {{{1
    get_updates_by_subject(Subject, false).

get_updates_by_subject(Subject, Archive) when is_list(Subject) ->  % {{{1
    get_updates_by_subject(list_to_binary(Subject), Archive);
get_updates_by_subject(Subject, false) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(message,
                                      [{#message{status='$1',
                                                 enc='$2',
                                                 subject=Subject,
                                                 _='_'},
                                        [{'and',
                                          {'/=', '$1', archive},
                                          {'/=', '$2', 6}}],
                                        ['$_']}])
                end);
get_updates_by_subject(Subject, true) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(message,
                                      [{#message{status='$1',
                                                 enc='$2',
                                                 subject=Subject,
                                                 _='_'},
                                        [{'and',
                                          {'==', '$1', archive},
                                          {'/=', '$2', 6}}],
                                        ['$_']}])
                end).
get_updates_by_user(UID) when is_list(UID) ->   % {{{1
    get_updates_by_user(list_to_binary(UID)); 
get_updates_by_user(UID) ->   % {{{1
    transaction(fun() ->
                        mnesia:select(message,
                                      [{#message{status='$1',
                                                 enc='$2',
                                                 folder=incoming,
                                                 from=UID,
                                                 _='_'},
                                        [{'/=', '$1', archive},
                                         {'or', 
                                          {'==', '$2', 3},
                                          {'==', '$2', 2}
                                         }],
                                        ['$_']}])
                end).

get_unread_updates() ->  % {{{1
    transaction(fun() ->
                        mnesia:select(message,
                                      [{#message{status=unread,
                                                 enc='$1',
                                                 folder=incoming,
                                                 subject='$2',
                                                 _='_'},
                                        [{'/=', '$1', 6},
                                         {'/=', '$2', <<"Update223322">>}],
                                        ['$_']}])
                end).

get_unread_ids() -> % {{{1
    Ms = get_unread_updates(),
    [M#message.hash || M <- Ms].

set_read(Id) ->  % {{{1
    transaction(fun() ->
                        case  mnesia:wread({message, Id}) of
                            [#message{status=S,
                                      folder=incoming} = U] ->
                                mnesia:write(message,
                                             U#message{status=read},
                                             write),
                                S;
                            _ ->
                                read
                        end
                end).

%%%
%% Contact routines
%%%

get_contact(undefined) ->  % {{{1
    {ok, Id} = next_id(db_contact),
    {ok, #db_contact{id=Id}};
get_contact(Id) ->  % {{{1
    transaction(fun() ->
                        [U] = mnesia:read(db_contact, Id),
                        U
                end).

get_contact_by_address(Address) ->  % {{{1
    transaction(fun() ->
                        case mnesia:index_read(db_contact, Address, #db_contact.address) of
                            L when length(L)>=1 ->
                                case coalesce_best_contact(L, false) of
                                    none -> coalesce_best_contact(L, true);
                                    U -> U
                                end;
                            _ ->
                                none
                        end
                end).

-spec coalesce_best_contact([#db_contact{}], boolean()) -> none | #db_contact{}.
%% @doc Returns the first contact it comes across that has an actual name, and
%% whether or not it's marked as Archived
coalesce_best_contact([User], Archive) when (User#db_contact.status==archive)==Archive ->  % {{{1
    User;
coalesce_best_contact([User | Rest], Archive) when (User#db_contact.status==archive)==Archive ->  % {{{1
    case User#db_contact.name of
        "unknown" -> coalesce_best_contact(Rest, Archive);
        "" -> coalesce_best_contact(Rest, Archive);
        _ -> User
    end;
coalesce_best_contact([User | Rest], Archive) ->  % {{{1 
    %% It is needed when contact is archived, but messages not
    case User#db_contact.name of
        "unknown" -> coalesce_best_contact(Rest, Archive);
        "" -> coalesce_best_contact(Rest, Archive);
        _ -> User
    end.


get_involved(Id) ->  % {{{1
    transaction(fun() ->
                        R = mnesia:match_object(#db_contact_roles{type=db_task, tid=Id, _='_'}),
                        lists:map(fun(#db_contact_roles{role=Role, contact=Contact}) ->
                                          [ #db_contact{name=Name, email=_Email}=C ] = mnesia:read(db_contact, Contact),
                                          {Name, Role, C}
                                  end, R)
                end).

get_involved_full(Id) -> % {{{1
    transaction(fun() ->
                        R = mnesia:match_object(#db_contact_roles{type=db_task, tid=Id, _='_'}),
                        lists:map(fun(ContactRole = #db_contact_roles{contact=Contact}) ->
                                          [ #db_contact{name=Name}] = mnesia:read(db_contact, Contact),
                                          {ContactRole, Name}
                                  end, R)
                end).

get_users(N) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_contact, [{#db_contact{name='$1'}, [], ['$1']}], N, read)
                end).

add_user_to_group(Group, User) ->  % {{{1
    transaction(fun() ->
                        mnesia:write(#db_group_members{group=Group, contact=User})
                end).

get_contacts_by_group(Group) ->  % {{{1
    get_contacts_by_group(Group, false).
get_contacts_by_group(my, false) -> % {{{1
    transaction(fun() -> 
                        mnesia:match_object(#db_contact{my=true, _='_'})
                end);
get_contacts_by_group(all, false) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_contact, [{#db_contact{my=false,status='$1', _='_'}, [{'/=', '$1', archive}],['$_']}])
                end);
get_contacts_by_group(all, true) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_contact, [{#db_contact{my=false,status='$1', _='_'}, [{'==', '$1', archive}],['$_']}])
                end);
get_contacts_by_group(Group, false) ->  % {{{1
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
get_contacts_by_group(Group, true) ->  % {{{1
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

get_groups_for_user(UID) ->  % {{{1
    transaction(fun() ->
                        GIDS = mnesia:select(db_group_members, [{#db_group_members{group='$1', contact=UID}, [], ['$1']}]),
                        iterate(db_group, GIDS)
                end).

get_notes_by_user(UID) ->  % {{{1
    transaction(fun() ->
                        mnesia:index_read(db_contact_note, UID, #db_contact_note.contact)
                end).

clear_roles(Type, Id) ->  % {{{1
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

get_contacts_by_name(Name) ->  % {{{1
    transaction(fun() ->
                        [C] = mnesia:select(db_contact, [{#db_contact{name=Name, status='$1', _='_'}, [{'/=', '$1', archive}], ['$_']}]),
                        C
                end).
backup(#db_contact{address=Address} = Cotact) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_contact, [{#db_contact{status='$1', _='_'}, [{'/=', '$1', 'archive'}], ['$_']}]) ++
                        mnesia:match_object(#privkey{address=Address, _='_'}) ++
                        mnesia:select(message,
                                      [{#message{from=Address,
                                                 status='$1',
                                                 _='_'},
                                        [{'/=', '$1', 'archive'}],
                                        ['$_']}])
                        %mnesia:match_object(#db_task{to=Address, _='_'}) ++
                end).
restore(Privkey, Contacts, Messages) ->  % {{{1
    transaction(fun() ->
                        ok=mnesia:write(privkey, Privkey, write),
                        lists:foreach(fun(Contact) ->
                                              mnesia:write(db_contact, Contact, write)
                                      end, Contacts),
                        [ #db_contact{bitmessage=MyAddress}] = mnesia:match_object(#db_contact{my=true, _='_'}),
                        lists:foreach(fun(#message{from=F} = Msg) when F == MyAddress ->
                                              ok=mnesia:write(message, Msg, write);
                                         (#message{to=F} = Msg) when MyAddress == F ->    
                                              ok=mnesia:write(message, Msg, write)
                                      end, Messages),
                        MyAddress
                end).

%%%
%% Group routines
%%%

get_groups() ->  % {{{1
    transaction(fun() ->
                        get_subgroup(undefined)
                end).

update_group_name(Id, Name) ->  % {{{1
    transaction(fun() ->
                        [G] = mnesia:read(db_group, Id),
                        mnesia:write(G#db_group{id=Id, name=Name})
                end).
save_subgroup(Id, Parent) ->  % {{{1
    transaction(fun() ->
                        [G] = mnesia:wread({db_group, Id}),
                        mnesia:write(G#db_group{subgroups=Parent})
                end).
delete_group(Id) ->  % {{{1
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

save_file(Path, #db_contact{id=UID}) ->  % {{{1
    Size = filelib:file_size(Path),
    Type = filename:extension(Path),
    <<FID:64/bytes, _/bytes>> = bm_message_encryptor:process_attachment(Path),
    File = #db_file{id=FID,
                    path=Path, 
                    size=Size,
                    date=date(),
                    status=uploaded,
                    user=UID,
                    type=Type
                   },
    transaction(fun() ->
                        mnesia:write(File)
                end),
    FID.

%%%
%% Attachment routines
%%%

get_attachments(Record) ->  % {{{1
    Type = element(1, Record),
    Id = element(2, Record),
    transaction(fun() ->
                        A = mnesia:select(db_attachment,
                                          [{#db_attachment{ file='$1',
                                                            type=Type,
                                                            tid=Id,
                                                            _='_'},
                                            [],
                                            ['$1']}]),
                        iterate(bm_file, A)
                end).

save_attachments(Record, Files) ->  % {{{1
    Type = element(1, Record),
    Id = element(2, Record),
    transaction(fun() ->
                        {ok, NId} = db:next_id(db_attachment),
                        save_attachment(Type, Id, sets:to_list(Files), NId)
                end).

get_files(FIDs) when is_list(FIDs) ->  % {{{1
    transaction(fun() ->
                        iterate(bm_file, FIDs)
                end);
get_files(true)  ->  % {{{1
    transaction(fun() ->
                        mnesia:select(bm_file, [{#bm_file{status=archive, _='_'}, [], ['$_']}])
                end);
get_files(false)  ->  % {{{1
    transaction(fun() ->
                        mnesia:select(bm_file, [{#bm_file{status='$1', _='_'}, [{'/=', '$1', archive}], ['$_']}])
                end).

get_owner(FID) ->  % {{{1
    transaction(fun() ->
                        [ #db_file{user=UID} ] = mnesia:read(db_file, FID),
                        [ #db_contact{bitmessage=Address} ] = mnesia:read(db_contact, UID),
                        Address
                end).

get_addresat(FID) ->  % {{{1
    transaction(fun() ->
                        Attachments = mnesia:read(db_attachment, FID),
                        iterate(db_contact, Attachments, fun(_, #db_attachment{type=Type, tid=TID}) ->
                                                                 [#db_contact{email=Email}] = mnesia:read(Type, TID),
                                                                 Email
                                                         end)
                end).

mark_downloaded(Id) ->  % {{{1
    transaction(fun() ->
                        [F] = mnesia:wread({ db_file, Id }),
                        mnesia:write(F#db_file{status=downloaded})
                end).

get_linked_messages(FID) when is_list(FID) ->  % {{{1
    get_linked_messages(wf:to_binary(FID));
get_linked_messages(FID) ->  % {{{1
    transaction(fun() ->
                        Messages = mnesia:table(message, {traverse,
                                                          {select,
                                                          [{
                                                            #message{status='$1',
                                                                     _='_'},
                                                            [{'/=', '$1', archive}],
                                                            ['$_']
                                                           }]
                                                         
                                                          }}),
                        qlc:e(qlc:q([ M || #message{attachments=As}=M <- Messages,
                                           As /= [],
                                           sets:is_element(FID, sets:from_list(As))
                                    ]))
                end).


%%%
%%  Admin functions
%%%

all_tasks() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_task{_='_'})
                end).
all_expenses() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_expense{_='_'})
                end).

all_updates() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_update{_='_'})
                end).

all_expense_taskss() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_expense_tasks{_='_'})
                end).
all_memberss() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_group_members{_='_'})
                end).

all_attachments() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_attachment{_='_'})
                end).

all_files() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_file{_='_'})
                end).

all_groups() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_group{_='_'})
                end).

all_contacts() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_contact{_='_'})
                end).

all_involved() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_contact_roles{_='_'})
                end).
%%%
%% Account routines
%%%

get_my_accounts() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(db_contact, #db_contact{my=true, _='_'}, read)
                end).

create_account(User, Passwd, Address) ->  % {{{1
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

transaction(Fun) ->  % {{{1
    case mnesia:transaction(Fun) of
        {aborted, R} ->
            {error, R};
        {atomic, '$end_of_table'} ->
            {ok, [], undefined};
        {atomic, R} ->
            {ok, R}
    end.
%%%
%% Helpers
%%%

save_attachment(_Type, _Id, [], _N) ->  % {{{1
    ok;
save_attachment(Type, Id, [#db_file{id=File}|Rest], N) ->  % {{{1
    save_attachment(Type, Id, [File|Rest], N);
save_attachment(Type, Id, [File|Rest], N) ->  % {{{1
    case mnesia:match_object(#db_attachment{file=File, type=Type, tid=Id, _='_'}) of
        [] ->
            mnesia:write(#db_attachment{id=N, file=File, type=Type, tid=Id}),
            save_attachment(Type, Id, Rest, N+1);
        _ ->
            save_attachment(Type, Id, Rest, N)
    end.

iterate(_, []) ->  % {{{1
    [];
iterate(Type, [Id|R]) ->  % {{{1
    mnesia:read(Type, Id) ++ iterate(Type, R).

iterate(_, [], _) ->  % {{{1
    [];
iterate(Type, [Id|R], Fun) ->  % {{{1
    Fun(Type, Id) ++ iterate(Type, R, Fun).

get_subgroup(G) ->  % {{{1
    Groups = mnesia:match_object(#db_group{subgroups=G, _='_'}),
    Sub = lists:map(fun(#db_group{id=N}=Gr) ->
                            Gr#db_group{subgroups=get_subgroup(N)}
                    end, Groups).

search_parent_rec(Id, PId) ->  % {{{1
    case mnesia:read(db_task, PId) of
        [] ->
            case mnesia:read(db_task_tree, PId) of
                [] ->
                    undefined;
                P ->
                    #db_task_tree{parent=PPId} = hd(lists:sort(fun(#db_task_tree{time=A}, #db_task_tree{time=B}) -> A > B end, P)),
                    search_parent_rec(Id, PPId)
            end;
        [#db_task{id=PId}] ->
            PId
    end.

verify_create_table({atomic, ok}) -> ok;  % {{{1
verify_create_table({aborted, {already_exists, _Table}}) -> ok. % {{{1

filter_messages_by_date({0, M, D}, #message{text=Data, enc=3, status=S}, A) when S /= archive->  % {{{1
    try
        #message_packet{time=TS} = binary_to_term(Data),
        case sugar:timestamp_to_datetime(TS) of
            {{_, MU, DU} = Date, _} when MU == M; DU == D ->
                A ++ [Date];
            _ ->
                A
        end
    catch
        error:_ ->
            A
    end;
filter_messages_by_date({Y, 0, 0}, #message{text=Data, enc=3, status=S}, A) when S /= archive->  % {{{1
    try
        #message_packet{time=TS} = binary_to_term(Data),
        case sugar:timestamp_to_datetime(TS) of
            {{Y, _, _} = Date, _} ->
                A ++ [Date];
            _ ->
                A
        end
    catch
        error:_ ->
            A
    end;
filter_messages_by_date(Date, #message{text=Data, enc=3, status=S}, A) when S /= archive->  % {{{1
    try
        #message_packet{time=TS} = binary_to_term(Data),
        case sugar:timestamp_to_datetime(TS) of
            {Date, _} ->
                A ++ [Date];
            _ ->
                A
        end
    catch
        error:_ ->
            A
    end;
filter_messages_by_date(_,_, A) ->  % {{{1
    A.

get_by_date(FilePred, TaskRe, Date) ->  % {{{1
    transaction(fun() ->
                        FilesH = mnesia:table(db_file, [{traverse, {select, [{#db_file{date={'$1', '$2', '$3'}, status='$4', _='_'}, [FilePred, {'/=', '$4', archive}],['$_']}]}}]),
                        Msg = mnesia:foldl(fun(Upd, A) -> filter_messages_by_date(Date, Upd, A) end, [], message),
                        TasksH1 = mnesia:table(db_task, [{traverse, {select, [{#db_task{status='$1', _='_'}, [ {'/=', '$1', archive}],['$_']}]}}]),
                        TasksH2 = qlc:q([T || #db_task{due=DT}=T <- TasksH1,
                                              re:run(DT, TaskRe) /= nomatch]),
                        FromFilesH = qlc:q([DT || #db_file{date=DT} <- FilesH], [unique]),
                        FromTasksH = qlc:q([sugar:date_from_string(DT) || #db_task{due=DT} <- TasksH2], [unique]),


                        Rest = qlc:e(qlc:append([FromFilesH, FromTasksH])),
                        lists:usort(Msg ++ Rest)

                end).

check_roles(Terms, Fun) ->  % {{{1
    search:check_roles(Terms, fun() -> {ok, []} end, Fun).

