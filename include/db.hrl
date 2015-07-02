-record(db_group,{id, name, subgroups}).
-record(db_group_members, {group, contact}).

-record(db_contact,
        {id,
         name="User ",
         email="",
         phone="",
         photo="undefined.png",
         bitmessage,
         address,
         my=false,
         status}).
-record(db_contact_note, {id, contact, datetime, text=""}).
-record(db_contact_roles,{id, type, tid, role, contact}).

-record(db_task,{id, due, name, text="", parent, status=new, changes=[]}).
-record(db_task_change, {address, datetime, field, new_value}).
-record(db_task_tree, {task, parent, time, visible=false}).

-record(db_file,{id, path, type, user, date, status, size}).
-record(db_attachment,{id, file, type, tid}).

-record(db_expense,{id, name, date, type=expense, text, amount, status, to, from}).
-record(db_expense_tasks, {expense, task}).

-record(db_update,{id, subject, from, to,  text, date, status}).
-record(db_search, {name="", text}).
