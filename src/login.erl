%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (login).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/login.html" }.

title() -> "Hello from login.erl!".

body() -> 
    [
        #textbox{id=login, placeholder="Email",class="input-block-level", next=passwd},
        #password{id=passwd, placeholder="Password", class="input-block-level", next=remember},
        #checkbox{id=remember, text="Remember my password",  checked=false},
        #button{html_encode=false, class="pull-right", text="<i class='icon-arrow-right'></i>", postback=login}
        ].
        
        
        
        
	
event(login) ->
    User = wf:q(login),
    Passwd = wf:q(passwd),
    case db:get_account(User, Passwd) of 
        {ok, []} ->
            {ok, U} = db:create_account(User, Passwd),
            wf:user(U),
            wf:redirect_from_login("/");
        {ok, [U]} ->
            wf:user(U),
            wf:redirect_from_login("/")
    end.
