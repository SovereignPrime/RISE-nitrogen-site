%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (textb).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Hello from textb.erl!".

body() -> 
    #sigma_search{tag=search, placeholder="Search", delegate=?MODULE}.

event(click) ->
    wf:insert_top(placeholder, "<p>You clicked the button!").

autocomplete_enter_event(Term, _Tag) ->
    io:format("Term ~p~n", [Term]),
    {ok, Contacts} = db:all_contacts(),
    List = [{struct, [{id, Id}, {label, wf:to_binary(Name ++ " - " ++ wf:to_list(Email))}, {value, wf:to_binary(Name)}]} || #db_contact{id=Id, name=Name, email=Email} <- Contacts, string:str(string:to_lower(wf:to_list(Name) ++ " - " ++ wf:to_list(Email)), string:to_lower(Term)) > 0],
    mochijson2:encode(List).
autocomplete_select_event({struct, [{<<"id">>, K}, {<<"value">>, V}]} = Selected, _Tag) ->
    io:format("Selected ~p~n", [Selected]),
    wf:session(V, wf:to_integer(K)).
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
                                            #link{text=wf:f("~s (~s) - ~s", [Name, Email, Grs]), postback={db_contact, Id}}
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
                                            #link{text=wf:f("~s (~s) - ~40s", [Subject, Name, TextS]), postback={db_update, Id}}
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
                                                #link{text=wf:f("~s (~s) - ~s", [Subject, Date, TextS]), postback={db_task, Id}}
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
                                                                            #link{text=wf:f("~s (~s) - ~s", [Subject, sugar:format_file_size( Size ), sugar:date_format(Date)]), postback={db_file, Id}}
                                                                            ]}
                            end, Files),
                         "</dd>"]
                end,
                "</dl>"
                ]},
                    
    {length(Contacts) + length(Messages) + length(Tasks) + length(Files), #panel{class="", body=[
                Out
                ]}}.  

