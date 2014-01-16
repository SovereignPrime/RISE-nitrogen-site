%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (relationships).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("pat/include/pat.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> "<i class='icon-user icon-large'></i>".

buttons() ->
    #panel{id=buttons, class='row-fluid', body=[

            #panel{class='span9 offset1', body=[
                    #panel{class="row-fluid", body=[
                            #link{id=archive, class='span2', body="<i class='icon-envelope-alt'></i> Invite via email", postback=invite},
                            %#panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                            common:render_filters(),
                            %#panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                            #link{id=archive, class='span2', body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}
                            ]}
                    ]}]}.

left() ->
    wf:session(current_group_id, all),
    {ok, Users} = db:get_contacts_by_group(all),
    [
        #panel{id=group_list, class="span2", body=[render_group_list(false)]},
                      #panel{id=user_list, class="span2", body=render_contact_list(Users)}
                      ].

render_group_list(Archive) ->
    {ok, Groups} = db:get_groups(),
    G = wf:session(current_group_id),
    wf:wire(wf:f("group~p", [G]), #add_class{class="active"}),
    [ #list{numbered=false,
          body=
            #group_item{gid=my, name="My accounts", sub=[], archive=Archive }
         },
    #list{numbered=false,
          body=
          #group_item{gid=all, name="All contacts", sub=Groups, archive=Archive }
         } ].

render_contact_list(Users) ->
    [
                #list{numbered=false,
                    body=
                        lists:map(fun(#db_contact{id=Id, name=Name}) ->
                                    #contact_li{uid=Id, name=Name, checked=false}
                            end, Users)
                        }

        ].
        

body() -> 
    {ok, Contact} = db:get_contact(wf:session(current_contact_id)),
    wf:session(current_contact, Contact),
    #panel{id=contact_panel, class="span8", body=contact_render(Contact)}.

contact_render(#db_contact{id=Id, name=Name, email=Email, phone=Phone,  address=Address, photo=Photo}) ->
    {ok, Tasks} = db:get_tasks_by_user(Id),
    {ok, Updates} = db:get_updates_by_user(Address),
    {ok, Groups} = db:get_groups_for_user(Id),
    [
        #vcard{id=vcard,name=Name, address=Address, email=Email, phone=Phone, photo=Photo, groups=[ N|| #db_group{name=N} <- Groups]},
        #table{class="table table-condensed", 
               rows=[
                #tablerow{cells=[
                        #tableheader{ body=[
                                "<i class='icon-calendar-empty'></i> Tasks"
                                ]},
                        #tableheader{},
                        #tableheader{body="Show all", class="cell-right"}
                        ]},
                lists:map(fun(#db_task{parent=Responsible, name=Name, due=Due}) ->
                            #taskrow{type=Responsible, name=Name, due=Due}
                    end, Tasks)

                ]},
            #singlerow{%class="table table-condensed", 
                       cells=[ #tableheader{ body=[
                        "<i class='icon-globe'></i> Updates"
                        ]},
                       #tableheader{},
                       #tableheader{body="Show all", class="cell-right"}
                             ]},
            #panel{class="span12", body=
                   [#update_element{collapse=paragraph, from=From, age="Age", text=Text} || #message{text=Text, from=From} <- Updates
                        ]}].

%%%
%% Event handlers
%%%

event(invite) ->
    #db_contact{email=MEmail} = wf:user(),
    #db_contact{email=REmail} = wf:session(current_contact),
    {ok, Server} = application:get_env(nitrogen, mail_server),
    {ok, Port} = application:get_env(nitrogen, mail_port),
    {ok, Login} = application:get_env(nitrogen, mail_login),
    {ok, Passwd} = application:get_env(nitrogen, mail_passwd),
    Con = pat:connect({Server, Port}, [{user, Login}, {password, Passwd}]),
    {ok, Text} = file:read_file("etc/invitation.tpl"),
    pat:send(Con, #email{sender=wf:to_binary(MEmail), 
                         recipients=[wf:to_binary( REmail )], 
                         headers=[{<<"content-type">>, <<"text/html">>}], 
                         subject= <<"Invitation to RISE">>, 
                         message= <<(wf:to_binary(MEmail))/binary, Text/binary>>});
event({archive, Rec}) ->
    db:archive(#db_contact{address=Rec}),
    Id = wf:session(current_group_id),
    {ok, Contacts} = db:get_contacts_by_group(Id),
    wf:update(group_list, render_group_list(false)),
    wf:update(user_list, render_contact_list(Contacts));
event({show_archive, true}) ->
    wf:replace(archive, #link{id=archive, class='span2', body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    Id = wf:session(current_group_id),
    {ok, Contacts} = db:get_contacts_by_group(Id, true),
    wf:update(group_list, render_group_list(true)),
    wf:update(user_list, render_contact_list(Contacts));
event({show_archive, false}) ->
    wf:replace(archive, #link{id=archive, class='span2', body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    Id = wf:session(current_group_id),
    {ok, Contacts} = db:get_contacts_by_group(Id, false),
    wf:update(group_list, render_group_list(false)),
    wf:update(user_list, render_contact_list(Contacts));
event({contact, Id}) ->
    {ok, Contact} = db:get_contact(Id),
    wf:session(current_contact, Contact),
    wf:update(contact_panel, contact_render(Contact));
event({group, Id, Archive}) ->
    {ok, Contacts} = db:get_contacts_by_group(Id, Archive),
    io:format("User ~p in ~p~n", [Contacts, Id]),
    wf:session(current_group_id, Id),
    wf:update(group_list, render_group_list(Archive)),
    wf:wire(wf:f("group~p", [Id]), #add_class{class="active"}),
    wf:update(user_list, render_contact_list(Contacts));
event({group_delete, Id, Archive}) ->
    db:delete_group(Id),
    wf:update(group_list, render_group_list(Archive));
event({group_rename, Id, Archive}) ->
    wf:update(wf:f("group_~p", [Id]), render_group_list(Archive));
event({write_to, Addr}) ->
    {ok, Id} = db:next_id(db_update),
    wf:session(current_update, #db_update{id=Id, to=[Addr]}),
    wf:redirect("/edit_update");
event(Click) ->
    io:format("~p~n",[Click]).

inplace_textbox_event({name, Id}, Name) ->
    Contact = wf:session(current_contact),
    db:save(Contact#db_contact{name=Name}),
    {ok, Contacts} = db:get_contacts_by_group(wf:session(current_group_id)),
    wf:update(user_list, render_contact_list(Contacts)),
    Name;
inplace_textbox_event({email, Id}, Name) ->
    Contact = wf:session(current_contact),
    db:save(Contact#db_contact{email=Name}),
    Name;
inplace_textbox_event({phone, Id}, Name) ->
    Contact = wf:session(current_contact),
    db:save(Contact#db_contact{phone=Name}),
    Name;
inplace_textbox_event({address, Id}, Name) ->
    Contact = wf:session(current_contact),
    db:save(Contact#db_contact{bitmessage=wf:to_binary(Name), address=wf:to_binary(Name)}),
    Name;
inplace_textbox_event(T, Name) ->
    io:format("Saved ~p tag ~p", [Name, T]),
    Name.

drop_event({contact, CId}, {subgroup, SId}) ->
    io:format("User: ~p Group: ~p~n", [CId, SId]),
    {ok, Contacts} = db:get_contacts_by_group(wf:session(current_group_id)),
    db:add_user_to_group(SId, CId),
    wf:update(user_list, render_contact_list(Contacts)),
    wf:update(contact_panel, contact_render(wf:session(current_contact)));
drop_event({group, CId, Archive}, {subgroup, all}) ->
    db:save_subgroup(CId, undefined),
    wf:update(group_list, render_group_list(Archive));
drop_event({group, CId, Archive}, {subgroup, SId}) when CId /= SId ->
    io:format("Group ~p to subgroup ~p~n", [SId, CId]),
    db:save_subgroup(CId, SId),
    wf:update(group_list, render_group_list(Archive));
drop_event(G, P) ->
    io:format("D&D ~p to ~p~n", [G, P]).

incoming() ->
    Id = wf:session(current_group_id),
    {ok, Contacts} = db:get_contacts_by_group(Id),
    wf:update(user_list, render_contact_list(Contacts)).
