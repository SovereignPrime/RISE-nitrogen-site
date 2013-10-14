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
    [
        #panel{class="span2", body=[
                #list{numbered=false,
                    body=[
                        #listitem{text="All contacts"},
                        #listitem{text="Most contacted"},
                        group_list()
                ]}
                
                ]},
        #panel{class="span2", body=[
                #list{numbered=false,
                    body=[
                        #listitem{class="clearfix", body=[
                                #checkbox{id=john, class="pull-left", style="margin-right: 15px;", text=" John Smith", postback=test, checked=false}
                                ]},
                        #listitem{class="clearfix", body=[
                                #checkbox{id=john, class="pull-left", style="margin-right: 15px;", text=" John Smith", postback=test, checked=false}
                                ]},
                        #listitem{class="clearfix", body=[
                                #checkbox{id=john, class="pull-left", style="margin-right: 15px;", text=" John Smith", postback=test, checked=false}
                                ]}
                        ]}
                ]}

        ].
        

body() -> 
    #panel{class="span8", body=
    [
            #vcard{name="John Smith", address="132 Pavlin st.", email="test@test.org", phone="+7 231 123456787"},
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
                    #taskrow{type="Responsible", name="Example task 1", due="1 Sep 2013"},
                    #taskrow{type="Accountable", name="Example task 2", due="17 Sep 2013"},
                    #taskrow{type="Responsible", name="Example task 3", due="14 Sep 2013"},
                    #taskrow{type="Responsible", name="Example task 4", due="10 Sep 2013"}

                    ]},
            #singlerow{%class="table table-condensed", 
                       cells=[ #tableheader{ body=[
                        "<i class='icon-globe'></i> Updates"
                        ]},
                       #tableheader{},
                       #tableheader{body="Show all", class="cell-right"}
                             ]},
            #panel{class="span12", body=
                   [ E || _ <- lists:seq(1,3), E <- [
                            #update_element{collapse=paragraph, from="Lorem ipisium", age="3 days", text="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut convallis egestas neque, sit amet mollis nisi tincidunt in. Proin fringilla sem vitae enim egestas, ut rutrum diam hendrerit. Nulla facilisi. Curabitur eleifend libero quam, sit amet sodales odio porttitor eget. Integer sit amet consequat magna. Ut eget tempus augue. Donec sodales suscipit ipsum, sed interdum nisl tincidunt a. In pretium mi ac viverra auctor. Nam dapibus interdum lectus et posuere."}
                            ]]}
            ]}.    

%%%
%% Event handlers
%%%

event(Click) ->
    io:format("~p~n",[Click]).

inplace_textbox_event({group, Id}, Name) ->
    db:update_group_name(Id, Name),
    Name.

drop_event({group, CId}, {subgroup, SId}) ->
    db:save_subgroup(CId, SId).
%%%
%% Helpers
%%%

group_list() ->
    {ok, Groups} = db:get_groups(),
    io:format("~p~n", [Groups]),
    #list{numbered=false,
          body=lists:map(fun(#db_group{id=Id, name=Name, subgroups=Sub}) ->
                    #group_item{gid=Id, name=Name, sub=Sub}
            end, Groups)
         }.
          



