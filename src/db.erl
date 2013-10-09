-module(db).
-compile([export_all]).
-include("db.hrl").

install()->
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

get_task() ->
    transaction(fun() ->
                mnesia:read(db_task, mnesia:last(db_task))
            end).

get_task(Id) -> 
    transaction(fun() ->
                mnesia:read(db_task, Id)
            end).
    
all_tasks() ->
    transaction(fun() ->
                mnesia:match_object(#db_task{_='_'})
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

transaction(Fun) ->
   case mnesia:transaction(Fun) of
        {error, R} ->
            {error, R};
        {atomic, '$end_of_table'} ->
            {ok, [], undefined};
        {atomic, R} ->
            {ok, R}
    end.
