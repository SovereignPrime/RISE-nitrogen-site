
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2013. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

-module(wx_interface).

-behaviour(wx_object).

%% Client API
-export([start/0]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
        parent,
        icon,
        menu,
        config
	}).

start() ->
    wx_object:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    WX = wx:new(),
    %inets:start(),
    wx:batch(fun() -> do_init(WX) end).
    %wxWindow:should(Frame).

do_init(Config) ->
    %Parent = proplists:get_value(parent, Config),  
    Panel = wxFrame:new(Config, -1, "RISE", []),
%
    %% Setup sizers
    %MainSizer = wxBoxSizer:new(?wxVERTICAL),
    %Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 %[{label, "wxHtmlWindow"}]),

    %% Create the wxHtmlWindow
    %HtmlWin = wxHtmlWindow:new(Panel, []),
    %% Load a file and display it
    %{ok, {_, _, HTML}} = httpc:request("http://localhost:8000"),
    %wxHtmlWindow:loadPage(HtmlWin, "http://localhost:8000"),
    Icon = wxTaskBarIcon:new(),
    wxTaskBarIcon:setIcon(Icon, wxIcon:new("site/static/img/rise.ico")),
    ok = wxTaskBarIcon:connect(Icon, taskbar_right_up), 
    Menu = wxMenu:new([]),

    %wxMenuItem:enable(wxMenu:append(Menu, wxMenuItem:new([{parentMenu, Menu}, 
    %                                                       {id, 1},
    %                                                      {text, "Start RISE"}])), [{enable, false}]),

    %wxMenu:append(Menu, wxMenuItem:new([{parentMenu, Menu}, 
    %                                  {id, 2},
    %                                   {text, "Stop RISE"}])),
    %wxMenu:appendSeparator(Menu),
    wxMenuItem:enable(wxMenu:append(Menu, wxMenuItem:new([{parentMenu, Menu}, 
                                                           {id, 3},
                                                          {text, "Start Torrent"}])), [{enable, false}]),

    wxMenu:append(Menu, wxMenuItem:new([{parentMenu, Menu}, 
                                      {id, 4},
                                       {text, "Stop Torrent"}])),
    wxMenu:appendSeparator(Menu),
    wxMenu:append(Menu, wxMenuItem:new([{parentMenu, Menu}, 
                                      {id, 5},
                                       {text, "Show RISE"}])),
    wxMenu:appendSeparator(Menu),
    wxMenu:append(Menu, wxMenuItem:new([{parentMenu, Menu}, 
                                      {id, 6},
                                       {text, "Quite RISE"}])),
    %wxHtmlWindow:loadFile(HtmlWin, "site/templates/bare.html"),

    %% Add to sizers
    % wxSizer:add(Sizer, HtmlWin, [{flag, ?wxEXPAND}, {proportion, 1}]),
    % wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion, 1}]),

    %wxHtmlWindow:connect(HtmlWin, command_html_link_clicked, [{skip,true}]),

    %wxPanel:setSizer(Panel, MainSizer),
    %wxWindow:show(Panel),
    {Panel, #state{parent=Icon, menu=Menu, icon=Icon, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxHtmlLink{linkInfo = #wxHtmlLinkInfo{href=Link}}},
	     State = #state{}) ->
    io:format("Link ~s~n", [Link]),
    {noreply, State};
handle_event(#wx{event=#wxTaskBarIcon{type=taskbar_right_up}},
	     State = #state{menu=Menu, icon=Icon}) ->
    wxTaskBarIcon:popupMenu(Icon, Menu),
    ok = wxFrame:connect(Icon, command_menu_selected), 
    {noreply, State#state{menu=Menu}};
handle_event(#wx{id=4, event=#wxCommand{type=command_menu_selected}},
	     State = #state{menu=Menu, icon=Icon}) ->
    io:format("Item ~p~n", [wxMenu:findItem(Menu, 4)]),
    application:stop(etorrent_core),
    wxMenu:enable(Menu, 4, false),
    wxMenu:enable(Menu, 3, true),
    {noreply, State};
handle_event(#wx{id=3, event=#wxCommand{type=command_menu_selected}},
	     State = #state{menu=Menu, icon=Icon}) ->
    io:format("Item ~p~n", [wxMenu:findItem(Menu, 4)]),
    application:start(etorrent_core),
    wxMenu:enable(Menu, 3, false),
    wxMenu:enable(Menu, 4, true),
    {noreply, State};
%handle_event(#wx{id=2, event=#wxCommand{type=command_menu_selected}},
%	     State = #state{menu=Menu, icon=Icon}) ->
%    io:format("Item ~p~n", [wxMenu:findItem(Menu, 4)]),
%    application:stop(nitrogen),
%    wxMenu:enable(Menu, 2, false),
%    wxMenu:enable(Menu, 1, true),
%    {noreply, State};
%handle_event(#wx{id=1, event=#wxCommand{type=command_menu_selected}},
%	     State = #state{menu=Menu, icon=Icon}) ->
%    io:format("Item ~p~n", [wxMenu:findItem(Menu, 4)]),
%    application:start(nitrogen),
%    wxMenu:enable(Menu, 1, false),
%    wxMenu:enable(Menu, 2, true),
%    {noreply, State};
handle_event(#wx{id=6, event=#wxCommand{type=command_menu_selected}},
	     State = #state{menu=Menu, icon=Icon}) ->
    io:format("Item ~p~n", [wxMenu:findItem(Menu, 4)]),
    init:stop(),
    {noreply, State};
handle_event(Link,
	     State = #state{menu=Menu, icon=Icon}) ->
    io:format("Link ~p ~p~n", [Link, Menu]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

