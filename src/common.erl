-module(common).
-compile([export_all]).
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").
-include("protokol.hrl").

main() ->  % {{{1
    PWD = os:getenv("ROOTDIR"),
    Timeout = application:get_env(nitrogen, db_timeout, 300),
    case mnesia:wait_for_tables([db_group], Timeout) of
        ok ->
            case wf:user() of
                'undefined' ->
                    case db:get_my_accounts() of 
                        {ok, []} ->
                            wf:redirect("/legal");
                        {ok, [U]} ->
                            db:update(),
                            wf:user(U),
                            main()
                    end;
                R ->
                    {ok, Pid} = wf:comet_global(fun  incoming/0, incoming),
                    Online = bitmessage:online(),
                    Pid! {status, Online},
                    receiver:register_receiver(Pid),
                    T = #template {file=PWD ++ "/site/templates/bare.html" },
                    wf:wire(backup_path,
                            #event{type=change,
                                   postback=backup_download,
                                   delegate=?MODULE}),
                    wf:wire('to_files',
                            #event{type=click,
                                   postback={to_files, undefined},
                                   delegate=?MODULE}),
                    wf:wire('to_relationships',
                            #event{type=click,
                                   postback={to_group, undefined},
                                   delegate=?MODULE}),
                    wf:wire('to_tasks',
                            #event{type=click,
                                   postback={to_task, undefined},
                                   delegate=?MODULE}),
                    wf:wire('to_expenses',
                            #event{type=click,
                                   postback={to_expense, undefined},
                                   delegate=?MODULE}),
                    wf:wire('to_updates',
                            #event{type=click,
                                   postback={to_message, undefined},
                                   delegate=?MODULE}),
                    wf:wire('new_contact',
                            #event{type=click,
                                   postback=add_contact,
                                   delegate=?MODULE}),
                    wf:wire('new_group',
                            #event{type=click,
                                   postback=add_group,
                                   delegate=?MODULE}),
                    wf:wire('new_task',
                            #event{type=click,
                                   postback=add_task,
                                   delegate=?MODULE}),
                    wf:wire('new_expense',
                            #event{type=click,
                                   postback=add_expense,
                                   delegate=?MODULE}),
                    wf:wire('new_update',
                            #event{type=click,
                                   postback=add_update,
                                   delegate=?MODULE}),
                    wf:wire(#event{type=timer,
                                   delay=600000,
                                   actions=#script{
                                              script="location.reload();"
                                             }
                                  }),
                    T
            end;
        {timeout, _} ->
            wf:redirect("/legal")
    end.

unread() -> % {{{1
    {ok, New} = db:get_unread_updates(),
    #span{id=count, class='label label-inverse',text=wf:f("~p new", [length(New)])}.

connection_status(N) when N > 0, % {{{1
                          N /= undefined ->
    "<script type='text/javascript'>" ++
    "$('.tooltip').remove();" ++
    "</script>" ++
    "<i class='icon icon-circle'></i> net" ++
    "<script type='text/javascript'>" ++
    "$('.wfid_connection').tooltip({placement: 'right'});" ++
    "</script>";
connection_status(_N) -> % {{{1
    "<script type='text/javascript'>" ++
    "$('.tooltip').remove();" ++
    "</script>" ++
    "<i class='icon icon-circle-blank'></i> net"++
    "<script type='text/javascript'>" ++
    "$('.wfid_connection').tooltip({placement: 'right'});" ++
    "</script>".

search() -> %{{{1
    #sigma_search{tag=search, 
                  placeholder="Search", 
                  class="input-append input-prepend input-block-level search", 
                  textbox_class="",
                  search_button_class="btn btn-inverse search-btn", 
                  search_button_text="<i class='icon icon-search'></i>",
                  x_button_class="search-x",
                  clear_button_class="pull-right btn btn-inverse",
                  clear_button_text="<i class='icon icon-remove'></i>",
                  results_summary_class="search-results span10",
                  delegate=?MODULE}.

render_files() -> % {{{1
    AttachmentsIDs = sets:to_list(wf:session_default(attached_files, sets:new())),
    {ok, Attachments} = db:get_files(AttachmentsIDs),
    #panel{id=files,
           class="span12",
           body=[
                 #panel{ class="row-fluid",
                         body=[
                               "<i class='icon-file-alt'></i> Attachments",
                               #br{},
                               #rise_upload{id=attachments,
                                            tag=filename,
                                            delegate=common,
                                            droppable_text="Drag and drop files here"
                                           },
                               #link{body="<i class='icon-th-large'></i> Select from my files",
                                     postback=add_file,
                                     new=false}
                              ]},
                 #br{},
                 lists:map(fun(#bm_file{path=Path,
                                        name=FName,
                                        size=Size,
                                        time={Date, _Time},
                                        hash=Id,
                                        status=Status}) ->
                                   #attachment{fid=Id,
                                               filename=Path ++ "/" ++ FName,
                                               size=Size,
                                               time=Date,
                                               status=Status}
                           end,
                           Attachments)
                ]}.

sigma_search_event(search, Terms) -> % {{{1
    TermsD = dict:from_list(Terms),
    {NTerms, Results} = search:terms(TermsD),
    Bs = lists:map(fun({"Date", Date}) ->
                           #sigma_search_badge{type="Date", text=sugar:date_format(Date)};
                      ({"Daterange", {SDate, EDate}}) ->
                           #sigma_search_badge{type="Daterange", text=sugar:date_format(SDate) 
                                               ++ " " ++
                                               sugar:date_format(EDate)
                                              };
                      ({"Group", Group}) ->
                           #sigma_search_badge{type="Group", text=Group};
                      ({"Term", _}) ->
                           "";
                      ({Type, Text}) ->
                           #sigma_search_badge{type=Type,
                                               text=Text,
                                               dropdown=[
                                                         "Contact",
                                                         "Responsible",
                                                         "Accountable",
                                                         "Consulted",
                                                         "Informed"
                                                        ]}
                   end, dict:to_list(NTerms)),

    {Bs,
     #panel{class="",
            body=[
                  Results,
                  #panel{body=#link{body="<i class='icon icon-filter'></i> Create filter with search", postback={save_filter_name, dict:to_list(NTerms)}, delegate=?MODULE}}
                ]}}.  
sigma_search_filter_event(search, Terms) ->  % {{{1
    wf:session(filter, dict:from_list(Terms)),
    search:check_roles(dict:from_list(Terms),
                       fun() ->
                               wf:redirect("/tasks")
                       end,
                       fun() -> 
                               wf:redirect("/")
                       end).
                               
sigma_search_filter_clear() ->  % {{{1
    wf:session(filter, undefined),
    wf:session(filter_name, undefined),
    wf:replace(filters, render_filters()),
    wf:replace(left, (wf:page_module()):left()).

render_filters() -> %{{{1
    case wf:session(filter_name) of
        undefined ->
            render_filters(nothing);
        F ->
            wf:wire(#script{script="$('.sigma_search_textbox').keydown()"}),
            wf:wire(#event{type=timer,
                           delay=300,
                           actions=#script{script="$('.sigma_search_results').hide()"}}),
            render_filters(F)
    end.
render_filters(Chosen) -> %{{{1
    {ok, Filters} = db:get_filters(),
    #panel{id=filters,
           class="btn-group",
           body=[
            #link{class=["btn",
                         "dropdown-toggle", 
                         "btn-link"],
                  style=case Chosen of
                             nothing ->
                                 "";
                             _ ->
                                 "color:#fff;background-color:#000;"
                         end, 
                  body=case Chosen of
                           nothing ->
                               "<i class='icon-filter'></i> Smart filter";
                           _ ->
                               "<i class='icon-filter'></i> " ++ Chosen
                       end,
                  data_fields=[{toggle, "dropdown"}],
                  url="#",
                  new=false},
            #list{numbered=false,
                  class="dropdown-menu",
                  body=lists:map(fun(#db_search{name=Name, text=Terms}) ->
                                         #listitem{ class="",
                                                    body=[
                                                          #link{class="pull-left", 
                                                                style="width:70%",
                                                                body=[#span{body=Name,
                                                                            actions=#event{type=mouseup,
                                                                                           postback={filter_load, Name, Terms},
                                                                                           delegate=?MODULE}
                                                                           },
                                                                      #span{class="pull-right",
                                                                            body="<i class='icon icon-remove'></i>",
                                                                            actions=#event{type=mouseup,
                                                                                           postback={filter_delete, Name},
                                                                                           delegate=?MODULE}
                                                                           }]}
                                                         ]}
                                 end, Filters)
                 }
                ]}.

render_help() ->  % {{{1
    #panel{ class='btn-group', body=[
        #link{class="btn dropdown-toggle btn-link", body="<i class='icon-question'></i> Help", data_fields=[{toggle, "dropdown"}], url="#", new=false},
        #list{numbered=false, class="dropdown-menu",body=[
            #listitem{body=[
				#email_link{text="For support: support@sovereignprime.com", email="support@sovereignprime.com"}
			]}
        ]}
    ]}.
	
settings_menu() -> %{{{1
    #panel{ class="btn-group", body=[
		#link{class="btn dropdown-toggle btn-link",
              body=[
                    #image{class="icon",
                           style="height:20px;vertical-align:middle;margin-top:-5px;",
                           image = "/img/id_card.svg"},
                    " My Profile"
                   ],
              data_fields=[{toggle, "dropdown"}],
              url="#", new=false},
		#list{numbered=false, class="dropdown-menu",body=[
			#listitem{ class="", body=[
				#link{text="View My Profile (and RISE ID)", postback=my_profile, delegate=?MODULE}
			]},
			#listitem{ class="", body=[
				#link{text="Wipe peers", postback=wrap_peers, delegate=?MODULE}
			]},
			#listitem{ class="", body=[
				#link{text="Backup user", postback=backup, delegate=?MODULE}
			]},
			#listitem{ class="", body=[
				#link{text="Restore user", postback=restore, delegate=?MODULE}
			]}
		]}
	]}.

event(my_profile) -> % {{{1
    maybe_unsaved(fun() ->
                      User = #db_contact{id=Id} = wf:user(),
                      wf:session(current_contact, User),
                      wf:session(current_contact_id,Id),
                      wf:redirect("/relationships")
              end);

event(add_group) -> %{{{1
    maybe_unsaved(fun() ->
                      {ok, Id} = db:next_id(db_group),
                      db:save(#db_group{
                                 id=Id,
                                 name="New group",
                                 subgroups=undefined
                                }),
                      wf:redirect("/relationships")
              end);
event(add_contact) -> %{{{1
    maybe_unsaved(fun() ->
                      {ok, Id} = db:next_id(db_contact),
                      wf:session(current_contact_id, Id),
                      Contact = #db_contact{
                                   id=Id,
                                   name="Contact Name"
                                  },
                      db:save(Contact),
                      wf:session(current_contact, Contact),
                      wf:redirect("/relationships")
              end);
event(add_task) -> %{{{1
    maybe_unsaved(fun() ->
                      wf:session(current_task, undefined),
                      wf:session(attached_files, sets:new()),
                      wf:redirect("/edit_task")
              end);
event(add_expense) -> %{{{1
    maybe_unsaved(fun() ->
                      {ok, Id} = db:next_id(db_expense),
                      wf:session(current_expense_id, Id),
                      wf:session(current_expense, #db_expense{id=Id}),
                      wf:session(attached_files, sets:new()),
                      wf:redirect("/edit_expense")
              end);
event(add_update) -> %{{{1
    maybe_unsaved(fun() ->
                      {ok, Id} = db:next_id(db_update),
                      wf:session(current_subject, undefined),
                      wf:session(current_update_id, Id),
                      wf:session(current_update,
                                 #db_update{id=Id}),
                      wf:session(attached_files, sets:new()),
                      wf:redirect("/edit_update")
              end);
event(check_all) -> %{{{1
    case wf:q(check_all) of
        "on" ->
            wf:replace(check, #checkbox{id=check,  postback=check_all, checked=true, delegate=common});
        undefined ->
            wf:replace(check, #checkbox{id=check,  postback=check_all, checked=false, delegate=common})
    end;
event({db_contact, Id}) -> %{{{1
    wf:session(current_contact_id, Id),
    wf:redirect("/relationships");
event({db_update, Id}) -> %{{{1
    wf:session(current_subject, Id),
    wf:redirect("/");
event({db_task, Id}) -> %{{{1
    wf:session(current_task_id, Id),
    {ok, [ Task ]} = db:get_task(Id),
    wf:session(current_task, Task),
    wf:redirect("/tasks");
event({db_file, Id}) -> %{{{1
    wf:redirect("/files");
event({search, Term}) -> %{{{1
    wf:set(".sigma_search_textbox", Term),
    wf:wire(#script{script="$('.sigma_search_textbox').keydown()"});
event({save_filter_name, Terms}) -> %{{{1
    wf:insert_bottom("body",
                     #popup{id=save_filter_name,
                            header="Save filter name...",
                            body=#panel{style="align:center;",
                                        class="input-append",
                                        body=[
                                              #textbox{id=filter_name}, 
                                              #button{id=ok, 
                                                      class="btn btn-link",
                                                      body=[
                                                            "<i class='icon icon-ok'></i>"
                                                           ],
                                                      postback={save_filter, Terms},
                                                      delegate=?MODULE
                                                     }
                                             ]}
                                  }),
    wf:wire(#event{target=save_filter_name,
                   postback={show, save_filter_name},
                   delegate=element_popup});

event({save_filter, Terms}) -> %{{{1
    Name = wf:q(filter_name),
    db:save(#db_search{text=Terms, name=Name}),
    wf:replace(filters, render_filters()),
    wf:wire(#event{postback={close, save_filter_name}, delegate=element_popup}),
    wf:wire(#script{script="$('.sigma_search_x_button').click()"});
event({filter_delete, Name}) ->  % {{{1
    db:delete(db_search, Name),
    sigma_search_filter_clear(),
    wf:replace(filters, render_filters());
event({filter_load, Name, Terms}) ->  % {{{1
    wf:session(filter_name, Name),
    wf:session(filter, Terms),
    Bs = lists:map(fun({"Date", Date}) ->
                           #sigma_search_badge{type="Date", text=sugar:date_format(Date)};
                      ({"Daterange", {SDate, EDate}}) ->
                           #sigma_search_badge{type="Daterange", text=sugar:date_format(SDate) 
                                               ++ " " ++
                                               sugar:date_format(EDate)
                                              };
                      ({"Group", Group}) ->
                           #sigma_search_badge{type="Group", text=Group};
                      ({"Term", T}) ->
                           wf:set(sigma_search_textbox, T),
                           "";
                      ({Type, Text}) ->
                           #sigma_search_badge{type=Type,
                                               text=Text,
                                               dropdown=[
                                                         "Contact",
                                                         "Responsible",
                                                         "Accountable",
                                                         "Consulted",
                                                         "Informed"
                                                        ]}
                   end, Terms),
    wf:replace(filters, render_filters(Name)),
    wf:replace(sigma_search_badges, Bs),
    wf:wire(#script{script="$('.sigma_search_textbox').keydown()"}),
    wf:wire(#script{script="$('.sigma_search_button').click()"});
    
    
event({reply, Subject, To}) -> % {{{1
    {ok, Id} = db:next_id(db_update),
    wf:session(current_subject, Subject),
    wf:session(current_update, #db_update{id=Id, to=[ To ], subject=Subject}),
    wf:session(attached_files, undefined),
    wf:redirect("/edit_update");

event({to_message, undefined}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:redirect("/")
                  end);

event({to_message, UID}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:info("UID: ~p~n", [UID]),
                          wf:session(current_update_id, UID),
                          {ok, #message{subject=Subject}} = db:get_update(UID),
                          wf:session(current_subject, Subject),
                          wf:redirect("/")
                  end);

event({to_contact, ID}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:session(current_contact_id, ID),
                          wf:redirect("/relationships")
                  end);

event({to_group, ID}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:session(current_group_id, ID),
                          wf:redirect("/relationships")
                  end);

event({to_files, _ID}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:redirect("/files")
    end);

event({to_task, undefined}) -> % {{{1
    maybe_unsaved(fun() ->
                          wf:redirect("/tasks")
    end);

event({to_task, Id}) -> % {{{1
    maybe_unsaved(fun() ->
                          wf:session(current_task_id, Id),
                          {ok, [ Task ]} = db:get_task(Id),
                          wf:session(current_task, Task),
                          wf:redirect("/tasks")
    end);

event(wrap_peers) -> %{{{1
    mnesia:clear_table(addr);
event(backup) -> %{{{1
    wf:wire(#script{script="init_download('" ++ wf:to_list(backup_path) ++ "')"}),
    wf:redirect("/raw?id=backup.rz&file=RISE_BACUP_" ++ sugar:date_string(date()) ++ ".rz");
event(backup_download) ->
    FD = wf:q(backup_path),
    error_logger:info_msg("Backup to file: ~s", [FD]),
    mnesia:backup(wf:f("~s", [FD]));
event(cancel) -> %{{{1
    wf:wire(#script{script="$('.modal').modal('hide')"}),
    wf:remove(".modal");
event(restore) -> %{{{1
    wf:insert_bottom("body",
                     #panel{class="modal fade",
                            body=[
                                  #panel{class="modal-header",
                                         body=[
                                               #button{class="btn-link pull-right",
                                                       text="x",
                                                       postback=cancel,
                                                       delegate=?MODULE},
                                               #h3{text="Restore user"}
                                              ]},
                                  #panel{class="modal-body",
                                         body=[
                                               #rise_upload{id=restore,
                                                            tag=restore,
                                                            delegate=common,
                                                            droppable_text="Drag and drop backup file here"
                                                           }
                                              ]}
                                 ]}),
    wf:wire(#script{script="$('.modal').modal('show')"});
event({unfold, #update_element{id=Id}=Update}) -> % {{{1
    wf:replace(Id,
               Update#update_element{collapse=false});

event({fold, #update_element{id=Id}=Update}) -> % {{{1
    wf:replace(Id, Update#update_element{collapse=true});
event(E) -> %{{{1
    io:format("Event ~p occured in ~p~n", [E, ?MODULE]).

dropevent(A, P) -> %{{{1
    io:format("Drag ~p drop ~p~n", [A, P]).

autocomplete_enter_event(Term, _Tag) -> %{{{1
    io:format("Term ~p~n", [Term]),
    {ok, Contacts} = db:get_contacts_by_group(all),
    List = [{struct, [{id, Id}, {label, wf:to_binary(Name ++ " - " ++ wf:to_list(Email))}, {value, wf:to_binary(Name)}]} || #db_contact{id=Id, name=Name, email=Email} <- Contacts, string:str(string:to_lower(wf:to_list(Name) ++ " - " ++ wf:to_list(Email)), string:to_lower(Term)) > 0],
    mochijson2:encode(List).
autocomplete_select_event({struct, [{<<"id">>, K}, {<<"value">>, V}]} = Selected, _Tag) -> %{{{1
    io:format("Selected ~p~n", [Selected]),
    wf:session(V, wf:to_integer(K)).

start_upload_event(_) -> %{{{1
    ok.
finish_upload_event(restore, FPath) -> %{{{1
    FID = filename:basename(FPath),
    common:restore(FID),
    timer:sleep(100),
    wf:redirect("/relationships");
finish_upload_event(filename, FPath) -> %{{{1
    io:format("File uploaded: ~p for ~p~n", [FPath, new]),
    User = wf:user(),
    File = db:save_file(FPath,User),
    AF = wf:session_default(attached_files, sets:new()),
    wf:session(attached_files, sets:add_element(File , AF)),
    wf:update(files, render_files()).

incoming() -> %{{{1
    receiver:register_receiver(self()),
    receive
        received ->
            wf:wire(#script{script="received();"}),
            wf:flush(),
            ?MODULE:incoming();
        sent ->
            wf:wire(#script{script="sent();"}),
            wf:flush(),
            ?MODULE:incoming();
        update ->
            (wf:page_module()):incoming(),
            wf:replace(count, unread()),
            wf:flush(),
            ?MODULE:incoming();
        {status, N} ->
            wf:update(connection, connection_status(N)),
            wf:flush(),
            ?MODULE:incoming()
    after
        1000 ->
            ?MODULE:incoming()
    end.

save_involved(Type, TId) -> %{{{1
    Involved = wf:qs(person),
    Role = wf:qs(responsible),
    io:format("~p ~p~n", [Involved, Role]),
    List = [ #db_contact_roles{type=Type,
                               tid=TId,
                               role=Role,
                               contact=Contact} || {Contact,
                                                    Role} <- lists:zip(Involved, Role),
                                                   Involved /= [[]],
                                                   Contact /= ""],
    db:clear_roles(Type, TId),
    lists:foreach(fun(#db_contact_roles{contact=C}=P) -> 
                {ok, NPId} = db:next_id(db_contact_roles),
                {ok, #db_contact{id=CID}}  = db:get_contacts_by_name(C),
                db:save(P#db_contact_roles{id=NPId, contact=CID})
        end, List).

send_messages(#db_update{subject=Subject, % {{{1
                         text=Text,
                         from=FID,
                         to=Contacts,
                         date=Date}=U) ->
    #db_contact{address=From} = wf:user(),
    wf:info("Sending ~p~n", [U]),
    {ok, Attachments} = db:get_attachments(U),
    MSG = term_to_binary(#message_packet{subject=Subject,
                                         text=Text,
                                         involved=[From | Contacts],
                                         time=bm_types:timestamp()}),

    AttachmentsPaths = lists:map(fun(#bm_file{hash=Hash}) ->
                                         Hash;
                                    (#db_file{id=Hash}) ->
                                         Hash
                                 end,
                                 Attachments),
    lists:foreach(fun(To) ->
                          error_logger:info_msg("Message to ~s sent subject ~s~n",
                                                [To,
                                                 Subject]),

                          bitmessage:send_message(From,
                                                  wf:to_binary(To),
                                                  wf:to_binary(Subject),
                                                  MSG,
                                                  AttachmentsPaths)
                  end,
                  Contacts);
send_messages(#db_task{id=UID, %{{{1
                       name=Subject,
                       text=Text,
                       due=Date,
                       parent=Parent,
                       status=Status,
                       changes=Changes} = U) ->
    {ok, Involved} = db:get_involved(UID),
    Contacts = [#role_packet{address=C, role=R} || {_, R, #db_contact{bitmessage=C}}  <- Involved],
    #db_contact{address=From} = wf:user(),
    {ok, Attachments} = db:get_attachments(U),
    AttachmentsPaths = lists:map(fun(#db_file{id=Hash}) ->
                                         Hash;
                                    (#bm_file{hash=Hash}) ->
                                         Hash
                                 end,
                                 Attachments),
    lists:foreach(fun(#role_packet{address=To}) when To /= From ->
                bitmessage:send_message(From,
                                        wf:to_binary(To), 
                                        wf:to_binary(Subject), 
                                        term_to_binary(#task_packet{id=UID,
                                                                    name=Subject,
                                                                    due=Date,
                                                                    text=Text,
                                                                    parent=Parent,
                                                                    status=Status,
                                                                    involved=Contacts,
                                                                    time=bm_types:timestamp(),
                                                                    changes=Changes}),

                                        AttachmentsPaths);
            (_) ->
                ok
        end, Contacts).

send_task_tree(Id, Parent, Time) -> %{{{1
    {ok, PInvolved} = db:get_involved(Parent),
    {ok, Involved } = db:get_involved(Id),
    PContact = sets:from_list(lists:map(fun({_, _, #db_contact{address=A}}) -> A end, PInvolved)),
    #db_contact{bitmessage=From} = wf:user(),
    lists:foreach(fun({_, _, #db_contact{bitmessage=To, my=false}}) ->
                          IsAdresat = sets:is_element(To, PContact),
                          if IsAdresat ->
                                  MSG = term_to_binary(#task_tree_packet{task=Id, parent=Parent, time=Time}),
                                  bitmessage:send_message(From, wf:to_binary(To), <<"Task tree">>, MSG, 6);
                              true -> ok
                          end;
                     (_) ->
                          ok
                  end, Involved).

restore(FID) -> %{{{1
    mnesia:restore(FID, [{clear_tables, mnesia:system_info(tables) -- [schema, addr]}, {skip_tables, [addr]}]),
    {ok, [Me]} = db:get_my_accounts(),
    wf:user(Me).

remove_duplicates(List) ->  % {{{1
	remove_duplicates(List, []).

remove_duplicates([], Acc) ->  % {{{1
	Acc;
remove_duplicates([H|T], Acc) ->  % {{{1
    case lists:member(H, Acc) of
        true -> remove_duplicates(T, Acc);
        false -> remove_duplicates(T, Acc ++ [H])
    end.

maybe_unsaved(Fun) ->  % {{{1
    case wf:state_default(unsaved, false) of
        true -> wf:wire(#confirm{text="The task is unsaved! Save it?",
                                 postback=save});
        false -> Fun()
    end.
