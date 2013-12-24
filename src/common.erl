-module(common).
-compile([export_all]).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").
-include("protokol.hrl").

main() -> 
    PWD = application:get_env(nitrogen, work_dir, "."),
    case wf:user() of
        'undefined' ->
            case db:get_my_accounts() of 
                {ok, []} ->
                    bitmessage:generate_address(self()),
                    receive
                        {address, Address} ->
                            {ok, U} = db:create_account("", true, Address),
                            wf:user(U)
                    end;
                {ok, [U]} ->
                    wf:user(U)
            end,
            main();
        R ->
            {ok, Pid} = wf:comet_global(fun  incoming/0, incoming),
            receiver:register_receiver(Pid),
            T = #template { file=PWD ++ "/site/templates/bare.html" },
            wf:wire('new_contact', #event{type=click, postback=add_contact, delegate=?MODULE}),
            wf:wire('new_group', #event{type=click, postback=add_group, delegate=?MODULE}),
            wf:wire('new_task', #event{type=click, postback=add_task, delegate=?MODULE}),
            wf:wire('new_expense', #event{type=click, postback=add_expense, delegate=?MODULE}),
            wf:wire('new_update', #event{type=click, postback=add_update, delegate=?MODULE}),
            T
    end.

search() ->
    #sigma_search{tag=search, 
                  placeholder="Search", 
                  class="input-append input-block-level search", 
                  textbox_class="input-block-level",
                  search_button_class="btn btn-inverse search-btn", 
                  search_button_text="<i class='icon icon-search'></i>",
                  x_button_class="search-x",
                  clear_button_class="search-x",
                  clear_button_text="",
                  results_summary_class="search-results",
                  delegate=?MODULE}.

render_files() ->
    {ok, Attachments} = db:get_files(sets:to_list(wf:session_default(attached_files, sets:new()))),
    #panel{id=files, class="span12", body=[
            #panel{ class="row-fluid", body=[
                    "<i class='icon-file-alt'></i> Attachments", #br{},
                    #upload{id=attachments, tag=filename, delegate=common, droppable=true, show_button=false, droppable_text="Drag and drop files here",  multiple=false}
                    ]},
            #br{},
            lists:map(fun(#db_file{path=Path, size=Size, date=Date, id=Id, status=Status}) ->
                        #attachment{filename=Path, size=Size, time=Date, status=Status}
                end, Attachments)
            ]}.

sigma_search_event(search, Term) ->
    {ok, {Contacts, Messages, Tasks, Files}} = db:search(Term),
    
    Out = #panel{body=[
                case Contacts of
                    [] ->
                        [];
                    _ ->
                        ["<dl class='dl-horizontal'>",
                        "<dt>Relationships:</dt><dd>",
                        lists:map(fun({#db_contact{id=Id, name=Name, email=Email}, Groups}) ->
                                    Grs = lists:foldl(fun(G, A) ->
                                                    A ++ ", " ++ G
                                            end, "", Groups),
                                    #panel{body=[
                                            #link{text=wf:f("~s (~s) - ~s", [Name, Email, Grs]), postback={db_contact, Id}, delegate=?MODULE}
                                            ]}
                            end, Contacts),
                         "</dd>"]
                end,
                case Messages of
                    [] ->
                        [];
                    _ ->
                        ["<dt>Messages:</dt><dd>",
                        lists:map(fun(#db_update{id=Id, subject=Subject, from=FID, text=Text}) ->
                                    {ok, #db_contact{name=Name}} = db:get_contact(FID),
                                    TextL = wf:to_list(Text),
                                    Pos = string:str(string:to_lower(TextL), string:to_lower(Term)),
                                    TextS = string:sub_string(TextL, Pos + 1),
                                    #panel{body=[
                                            #link{text=wf:f("~s (~s) - ~40s", [Subject, Name, TextS]), postback={db_update, Id}, delegate=?MODULE}
                                            ]}
                            end, Messages),
                         "</dd>"]
                end,
                case Tasks of
                    [] ->
                        [];
                    _ ->
                        ["<dt>Tasks:</dt><dd>",
                         lists:map(fun(#db_task{id=Id, name=Subject, due=Date, text=Text}) ->
                                        TextL = wf:to_list(Text),
                                        Pos = string:str(string:to_lower(TextL), string:to_lower(Term)),
                                        TextS = string:sub_string(TextL, Pos + 1),
                                        #panel{body=[
                                                #link{text=wf:f("~s (~s) - ~s", [Subject, Date, TextS]), postback={db_task, Id}, delegate=?MODULE}
                                                ]}
                            end, Tasks),
                         "</dd>"]
                end,

                 case Files of
                    [] ->
                        [];
                    _ ->
                        ["<dt>Files:</dt><dd>",
                        lists:map(fun(#db_file{id=Id, path=Subject, size=Size, date=Date}) ->
                                                                    #panel{body=[
                                                                            #link{text=wf:f("~s (~s) - ~s", [Subject, sugar:format_file_size( Size ), sugar:date_format(Date)]), 
                                                                                  postback={db_file, Id}, 
                                                                                  delegate=?MODULE}
                                                                            ]}
                            end, Files),
                         "</dd>"]
                end,
                "</dl>"
                ]},
                    
    {length(Contacts) + length(Messages) + length(Tasks) + length(Files), #panel{class="", body=[
                Out,
                #panel{body=#link{body="<i class='icon icon-filter'></i> Create filter with search", postback={save_filter, Term}, delegate=?MODULE}}
                
                ]}}.  
render_filters() ->
    {ok, Filters} = db:get_filters(),
    #panel{ class="btn-group span2", body=[
            #link{class="btn dropdown-toggle btn-link", body="<i class='icon-filter'></i> Smart filter", data_fields=[{toggle, "dropdown"}], url="#", new=false},
            #list{numbered=false, class="dropdown-menu",
                  body=
                  lists:map(fun(Term) ->
                            #listitem{ class="", body=[
                                    #link{text=Term, postback={search, Term}, delegate=?MODULE}
                                    ]}
                    end, Filters)
                 }
            ]}.

event(add_group) ->
    {ok, Id} = db:next_id(db_group),
    db:save(#db_group{
            id=Id,
            name="New group",
            subgroups=undefined
            }),
    wf:redirect("/relationships");
event(add_contact) ->
    {ok, Id} = db:next_id(db_contact),
    wf:session(current_contact, undefined),
    wf:session(current_contact_id, Id),
    db:save(#db_contact{
            id=Id,
            name="Contact Name"
            }),
    wf:redirect("/relationships");
event(add_task) ->
    wf:session(current_task, undefined),
    wf:session(attached_files, sets:new()),
    wf:redirect("/edit_task");
event(add_expense) ->
    {ok, Id} = db:next_id(db_expense),
    wf:session(current_expense_id, Id),
    wf:session(current_expense, #db_expense{id=Id}),
    wf:session(attached_files, sets:new()),
    wf:redirect("/edit_expense");
event(add_update) ->
    {ok, Id} = db:next_id(db_update),
    wf:session(current_subject, undefined),
    wf:session(current_update_id, Id),
    wf:session(current_update, #db_update{id=Id}),
    wf:session(attached_files, sets:new()),
    wf:redirect("/edit_update");
event(check_all) ->
    case wf:q(check_all) of
        "on" ->
            wf:replace(check, #checkbox{id=check,  postback=check_all, checked=true, delegate=common});
        undefined ->
            wf:replace(check, #checkbox{id=check,  postback=check_all, checked=false, delegate=common})
    end;
event({db_contact, Id}) ->
    wf:session(current_contact_id, Id),
    wf:redirect("/relationships");
event({db_update, Id}) ->
    wf:session(current_update_id, Id),
    wf:redirect("/");
event({db_task, Id}) ->
    wf:session(current_task_id, Id),
    {ok, [ Task ]} = db:get_task(Id),
    wf:session(current_task, Task),
    wf:redirect("/tasks");
event({db_file, Id}) ->
    wf:redirect("/files");
event({search, Term}) ->
    wf:set(".sigma_search_textbox", Term),
    sigma_search_event(search, Term),
    wf:wire(#script{script="$('.sigma_search_textbox').keydown()"});
event({save_filter, Term}) ->
    db:save(#db_search{text=Term}),
    wf:wire(#script{script="$('.sigma_search_x_button').click()"});
event(E) ->
    io:format("Event ~p occured in ~p~n", [E, ?MODULE]).

dropevent(A, P) ->
    io:format("Drag ~p drop ~p~n", [A, P]).

autocomplete_enter_event(Term, _Tag) ->
    io:format("Term ~p~n", [Term]),
    {ok, Contacts} = db:all_contacts(),
    List = [{struct, [{id, Id}, {label, wf:to_binary(Name ++ " - " ++ wf:to_list(Email))}, {value, wf:to_binary(Name)}]} || #db_contact{id=Id, name=Name, email=Email} <- Contacts, string:str(string:to_lower(wf:to_list(Name) ++ " - " ++ wf:to_list(Email)), string:to_lower(Term)) > 0],
    mochijson2:encode(List).
autocomplete_select_event({struct, [{<<"id">>, K}, {<<"value">>, V}]} = Selected, _Tag) ->
    io:format("Selected ~p~n", [Selected]),
    wf:session(V, wf:to_integer(K)).

start_upload_event(_) ->
    ok.
finish_upload_event(filename, FName, FPath, _Node) ->
    FID = filename:basename(FPath),
    io:format("File uploaded: ~p to ~p for ~p~n", [FName, FPath, new]),
    TName = wf:f("scratch/~s.torrent", [FID]),
    etorrent_mktorrent:create(FPath, undefined, TName),
    User = wf:user(),
    File = db:save_file(FName, FPath,User),
    AF = wf:session_default(attached_files, sets:new()),
    wf:session(attached_files, sets:add_element( FID , AF)),
    wf:update(files, render_files()).

incoming() ->
    receive
        update ->
            (wf:page_module()):incoming(),
            incoming()
    end.

save_involved(Type, TId) ->
    Involved = wf:qs(person),
    Role = wf:qs(responsible),
    io:format("~p ~p~n", [Involved, Role]),
    List = [ #db_contact_roles{type=Type, tid=TId, role=Role, contact=wf:session(wf:to_binary(Contact))} || {Contact, Role} <- lists:zip(Involved, Role), Involved /= [[]], Contact /= ""],
    db:clear_roles(Type, TId),
    lists:foreach(fun(P) -> 
                {ok, NPId} = db:next_id(db_contact_roles),
                db:save(P#db_contact_roles{id=NPId})
        end, List).

send_messages(#db_update{subject=Subject, text=Text, from=FID, to=Contacts, date=Date}=U) ->
    #db_contact{address=From} = wf:user(),
    InvolvedB =  <<"Involved:", << <<A/bytes, ";">> || A <- [From | Contacts]>>/bytes, 10>>,
    case db:get_attachments(U) of
        {ok, []} ->
            lists:foreach(fun(To) ->
                        bitmessage:send_message(From, wf:to_binary(To), wf:to_binary(Subject), <<(wf:to_binary(Text))/bytes, 10, InvolvedB/bytes>>, 2)
                end, Contacts);
        {ok, Attachments} -> 
            AttachmentsB = encode_attachments(Attachments),
            lists:foreach(fun(To) ->
                        bitmessage:send_message(From, wf:to_binary(To), wf:to_binary(Subject), <<(wf:to_binary(Text))/bytes, 10, InvolvedB/bytes,  AttachmentsB/bytes>>, 3)
                end, Contacts)
    end;
send_messages(#db_task{id=UID, name=Subject, text=Text, due=Date, parent=Parent, status=Status} = U) ->
    {ok, Involved} = db:get_involved(UID),
    Contacts = [#role_packet{address=C, role=R} || {_, R, #db_contact{bitmessage=C}}  <- Involved],
    #db_contact{address=From} = wf:user(),
    {ok, Attachments} = db:get_attachments(U),
    lists:foreach(fun(#role_packet{address=To}) when To /= From ->
                bitmessage:send_message(From,
                                        wf:to_binary(To), 
                                        wf:to_binary(Subject), 
                                        term_to_binary(#task_packet{id=UID, name=Subject, due=Date, text=Text, parent=Parent, status=Status, attachments=Attachments, involved=Contacts}),
                                        4);
            (_) ->
                ok
        end, Contacts).

encode_attachments(Attachments) ->
    AttachmentsL = lists:map(fun(#db_file{user=UID}=A) ->
                    {ok, #db_contact{bitmessage=Addr}} = db:get_contact(UID),
                    term_to_binary(A#db_file{user=Addr})
            end, Attachments),
    <<"Attachments:", << <<A/bytes,";">> || A <- AttachmentsL>>/bytes, 10>>.

get_torrent(FID) ->
    #db_contact{bitmessage=From} = wf:user(),
    {ok, To} = db:get_owner(FID),
    bitmessage:send_message(From, To, <<"Get torrent">>, wf:to_binary(FID), 6).
