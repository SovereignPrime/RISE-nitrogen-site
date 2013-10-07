-module(db).
-compile([export_all]).
-include("db.hrl").

install()->
    {atomic, ok} = mnesia:create_table(group, [{disc_copies, [node()]}, {attributes, record_info(fields, group)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(contact, [{disc_copies, [node()]}, {attributes, record_info(fields, contact)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(task, [{disc_copies, [node()]}, {attributes, record_info(fields, task)}, {type, ordered_set}]),
    {atomic, ok} = mnesia:create_table(file, [{disc_copies, [node()]}, {attributes, record_info(fields, file)}, {type, ordered_set}, {record_name, network_address}]),
    {atomic, ok} = mnesia:create_table(expense, [{disc_copies, [node()]}, {attributes, record_info(fields, expense)}, {type, ordered_set}, {record_name, expense}]),
    {atomic, ok} = mnesia:create_table(upd, [{disc_copies, [node()]}, {attributes, record_info(fields, upd)}, {type, ordered_set}, {record_name, upd}]),
    {atomic, ok} = mnesia:create_table(contact_roles, [{disc_copies, [node()]}, {attributes, record_info(fields, contact_roles)}, {type, ordered_set}, {record_name, contact_roles}]),
    {atomic, ok} = mnesia:create_table(attachment, [{disc_copies, [node()]}, {attributes, record_info(fields, attachment)}, {type, ordered_set}, {record_name, attachment}]).

new_task(Name, Due, Text, Linked) ->
    {atomic, ok} = mnesia:transaction(fun() ->
                    N = case mnesia:table_info(task, size) of
                        '$end_of_table' ->
                            0;
                        A -> 
                            A
                    end,
                    Task = #task{id=N+1,
                            name=Name,
                            due=Due,
                            text=Text,
                            linked=Linked
                                },
                    ok = mnesia:write(Task)
        end).
save_task(Id, Name, Due, Text, Linked) ->
    Task = #task{id=Id,
                 name=Name,
                 due=Due,
                 text=Text,
                 linked=Linked
                },
    io:format("Task to save: ~p~n", [Task]),
    {atomic, ok} = mnesia:transaction(fun() ->
                ok = mnesia:write(Task)
        end).
get_task(Id) -> 
    {atomic, [ Task ]} = mnesia:transaction(fun() ->
                    mnesia:read(task, Id)
            end),
    {ok, Task}.
    
all_tasks() ->
    {atomic, Tasks} = mnesia:transaction(fun() ->
                    mnesia:match_object(#task{_='_'})
            end),
    io:format("~p~n", [Tasks]).

get_users(N) ->
    {atomic, R} = mnesia:transaction(fun() ->
                mnesia:select(contact, [{#contact{name='$1'}, [], ['$1']}], N, read)
        end),
    R.

get_tasks(N) ->
    {atomic, R} = mnesia:transaction(fun() ->
                    mnesia:select(task, [{#task{_='_'}, [], ['$_']}], N, read)
        end),
    {ok, R}.
