-module(db).
-compile([export_all]).
-include("db.hrl").

install()->
    {atomic, ok} = mnesia:create_table(group, [{disc_copies, [node()]}, {attributes, record_info(fields, group)}, {type, set}]),
    {atomic, ok} = mnesia:create_table(contact, [{disc_copies, [node()]}, {attributes, record_info(fields, contact)}, {type, set}]),
    {atomic, ok} = mnesia:create_table(task, [{disc_copies, [node()]}, {attributes, record_info(fields, task)}, {type, set}]),
    {atomic, ok} = mnesia:create_table(file, [{disc_copies, [node()]}, {attributes, record_info(fields, file)}, {type, set}, {record_name, network_address}]),
    {atomic, ok} = mnesia:create_table(expense, [{disc_copies, [node()]}, {attributes, record_info(fields, expense)}, {type, set}, {record_name, expense}]),
    {atomic, ok} = mnesia:create_table(upd, [{disc_copies, [node()]}, {attributes, record_info(fields, upd)}, {type, set}, {record_name, upd}]),
    {atomic, ok} = mnesia:create_table(contact_roles, [{disc_copies, [node()]}, {attributes, record_info(fields, contact_roles)}, {type, set}, {record_name, contact_roles}]),
    {atomic, ok} = mnesia:create_table(attachment, [{disc_copies, [node()]}, {attributes, record_info(fields, attachment)}, {type, set}, {record_name, attachment}]).
