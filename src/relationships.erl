%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (relationships).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> "<i class='icon-user icon-large'></i>".

buttons() ->
    #panel{id=buttons, class='row-fluid', body=[

    #panel{class='span9 offset3', body=[
            #panel{class="row-fluid", body=[
                    #panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
                    #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                    #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                    #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                    ]}
            ]}]}.

left() ->
    wf:session(current_group_id, all),
    {ok, Users} = db:get_contacts_by_group(all),
    [
        #panel{id=group_list, class="span2", body=[render_group_list()]},
                      #panel{id=user_list, class="span2", body=render_contact_list(Users)}
                      ].

render_group_list() ->
    {ok, Groups} = db:get_groups(),
    G = wf:session(current_group_id),
    wf:wire(wf:f("group~p", [G]), #add_class{class="active"}),
    [ #list{numbered=false,
          body=
            #group_item{gid=my, name="My accounts", sub=[] }%[
         },
    #list{numbered=false,
          body=
          #group_item{gid=all, name="All contacts", sub=Groups }%[
            %#listitem{text="Most contacted"}
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
    {ok, Updates} = db:get_updates_by_user(Id),
    {ok, Groups} = db:get_groups_for_user(Id),
    [
        #vcard{id=vcard,name=Name, address=Address, email=Email, phone=Phone, photo=Photo, groups=[ N|| #db_group{name=N} <- Groups]},
        #table{class="table table-condensed", 
               rows=[
                #tablerow{cells=[
                        #tableheader{ body=[
                                #span{class=" icon-small icon-stack", html_encode=false, text="<i class='icon-calendar-empty icon-stack-base'></i><i class='icon-ok'></i>"},
                                "Tasks"
                                ]},
                        #tableheader{},
                        #tableheader{body="Show all", class="cell-right"}
                        ]},
                lists:map(fun(#db_task{parent=Responsible, name=TName, due=Due}) ->
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
                   [#update_element{collapse=paragraph, from=From, age=Age, text=Text} || #db_update{text=Text, subject=From, date=Age} <- Updates
                        ]}].

%%%
%% Event handlers
%%%

event({contact, Id}) ->
    {ok, Contact} = db:get_contact(Id),
    wf:session(current_contact, Contact),
    wf:update(contact_panel, contact_render(Contact));
event({group, Id}) ->
    {ok, Contacts} = db:get_contacts_by_group(Id),
    io:format("User ~p in ~p~n", [Contacts, Id]),
    wf:session(current_group_id, Id),
    wf:update(group_list, render_group_list()),
    wf:wire(wf:f("group~p", [Id]), #add_class{class="active"}),
    wf:update(user_list, render_contact_list(Contacts));
event({group_delete, Id}) ->
    db:delete_group(Id),
    wf:update(group_list, render_group_list());
event({group_rename, Id}) ->
    wf:update(wf:f("group_~p", [Id]), render_group_list());
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
drop_event({group, CId}, {subgroup, all}) ->
    db:save_subgroup(CId, undefined),
    wf:update(group_list, render_group_list());
drop_event({group, CId}, {subgroup, SId}) when CId /= SId ->
    io:format("Group ~p to subgroup ~p~n", [SId, CId]),
    db:save_subgroup(CId, SId),
    wf:update(group_list, render_group_list());
drop_event(G, P) ->
    io:format("D&D ~p to ~p~n", [G, P]).

incoming() ->
    Id = wf:session(current_group_id),
    {ok, Contacts} = db:get_contacts_by_group(Id),
    wf:update(user_list, render_contact_list(Contacts)).
