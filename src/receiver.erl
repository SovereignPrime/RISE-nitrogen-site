-module(receiver).

-behaviour(gen_server).
-include_lib("bitmessage/include/bm.hrl").
-include("db.hrl").
-include("protokol.hrl").

%% API
-export([
    start_link/0,
    register_receiver/1
    ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_receiver(Pid) ->
    gen_server:cast(?MODULE, {register, Pid}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    bitmessage:register_receiver(self()),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({register, Pid}, State) ->
    {noreply, State#state{pid=Pid}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({msg, Hash}, State) ->
    %io:format("~p~n", [Hash]),
    {ok, #message{from=From, to=To, subject=Subject, text=Text, enc=Enc} = Message}= bitmessage:get_message(Hash),
    FID = get_or_request_contact(From, From, To),
    {ok, #db_contact{id=ToID}} = db:get_contact_by_address(To),
    apply_message(Message, FID, ToID),
    State#state.pid ! update,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%
%%% Technical messages (not visible for users
%%%

apply_message(#message{from=BMF, to=BMT, subject= <<"Get vCard">>, text=Data, enc=6}, FID, ToID) ->
                       {ok,  #db_contact{name=Name, email=Email, phone=Phone, bitmessage=BM, address=Address} } = db:get_contact_by_address(Data),
                       bitmessage:send_message(BMT, BMF, <<"vCard">>, <<(wf:to_binary(Name))/bytes, ",", (wf:to_binary(Email))/bytes, ",", (wf:to_binary(Phone))/bytes, ",", (wf:to_binary(Address))/bytes, ",", (wf:to_binary(BM))/bytes>>, 6);
apply_message(#message{from=BMF, to=BMT, subject= <<"vCard">>, text=Data, enc=6}, FID, ToID) ->
    [Name, Email, Phone, Address, BM] = binary:split(Data, <<",">>, [global, trim]),
    {ok, Contact } = db:get_contact_by_address(BM),
    db:save(Contact#db_contact{name=Name, email=Email, phone=Phone, address=Address});

apply_message(#message{from=BMF, to=BMT, subject= <<"Get torrent">>, text=Data, enc=6}, FID, ToID) ->
    {ok,  F } = file:read_file("scratch/" ++ wf:to_list(Data) ++ ".torrent"),
    bitmessage:send_message(BMT, BMF, <<"torrent">>, <<Data/bytes, ";", F/bytes>>, 6);

apply_message(#message{from=BMF, to=BMT, subject= <<"torrent">>, text=Data, enc=6}, FID, ToID) ->
    [Id, Torrent] = binary:split(Data, <<";">>, [trim]),
    Path = wf:f(".new/~s.torrent", [Id]),
    file:write_file("scratch/" ++ Path, Torrent),
    etorrent:start(Path, {callback, fun() ->
                    db:mark_downloaded(wf:to_list(Id)),
                    file:rename("scratch/" ++ Path, wf:f("scratch/~s.torrent", [Id]))
        end});

apply_message(#message{from=BMF, to=BMT, subject= <<"Task tree">>, text=Data, enc=6}, FID, ToID) ->
    #task_tree_packet{task=Task, parent=Parent} = binary_to_term(Data),
    case db:get_task(Parent) of
        [] ->
            ok;
        _ ->
            db:save_subtask(Task, Parent)
    end;

apply_message(#message{from=BMF, to=BMT, subject= <<"Update223322">>, text=Data, enc=2}, FID, ToID) ->
    [BVSN, Torrent] = binary:split(Data, <<";">>, [trim]),
    {ok, CVSN} = application:get_key(nitrogen, 'vsn'),
    PWD = application:get_env(nitrogen, work_dir, "."),
    Home = application:get_env(nitrogen, home_dir, "."),
    OVSN = wf:to_integer(CVSN),
    VSN = wf:to_integer(BVSN),
    if VSN > OVSN  ->
           U = PWD ++ "/site/.update",
           file:make_dir(U),
           Path = wf:f("~s/~s.torrent", [U, BVSN]),
           file:write_file(  Path, base64:decode( Torrent )),
           etorrent:start(Path, {callback, fun() ->
                                                            io:format("Updating: ~p~n", [file:get_cwd()]),
                                                            {ok, ZData} = file:read_file(wf:f("~s/scratch/u_~s.tar.gz", [Home, BVSN])),
                                                            erl_tar:extract({binary, ZData}, [{cwd, U}, compressed]),
                                                            {ok, Mod} = compile:file(U ++ "/update"),
                                                            ok = Mod:main(PWD, Home),
                                                            file:rename(Path, wf:f("~s/scratch/~s.torrent", [Home, BVSN])),
                                                            os:cmd("rm -rf " ++ U)
                                                    end});
       true -> ok
    end;



%%%
%% Informational messages
%%%

apply_message(#message{from=BMF, to=BMT, subject=Subject, text=Data, enc=3}, FID, ToID)  ->
    {ok, Id} = db:next_id(db_update),

    #message_packet{text=Text, attachments=Attachments, involved=Involved} = binary_to_term(Data),
    Message = #db_update{id=Id, date=date(), from=FID, to=Involved -- [BMT], subject=wf:to_list(Subject), text=Text, status=unread},
    db:save(Message),
    Files = lists:map(fun(#db_file{id=I}=F) ->
                                    db:save(F#db_file{user=FID, status=received}),
                                    I
                    end, Attachments),
    db:save_attachments(Message, sets:from_list(Files));
apply_message(#message{from=BMF, to=BMT, subject=Subject, text=Data, enc=4}, FID, ToID)  ->
    #task_packet{id=UID, 
                 due=Due, 
                 %name=wf:to_list(Subject), 
                 text=Text, 
                 parent=Parent, 
                 status=Status, 
                 attachments=AttachmentsE, 
                 involved=Involved} = binary_to_term(Data),
    Task = case db:get_task(Parent) of
               {ok, []} ->
                   #db_task{id=UID, due=Due,  name=wf:to_list(Subject), text=Text, status=Status};
               {ok, _} ->
                   #db_task{id=UID, due=Due,  name=wf:to_list(Subject), text=Text, parent=Parent, status=Status}
           end,
    db:save(Task),
    db:clear_roles(db_task, UID),
    Attachments = lists:map(fun(#db_file{id=I}=F) ->
                                    db:save(F#db_file{user=FID, status=received}),
                                    I
                    end, AttachmentsE),
    db:save_attachments(Task, sets:from_list(Attachments)),
    lists:foreach(fun(#role_packet{address=A, role=R}) ->
                {ok, NPUID} = db:next_id(db_contact_roles),
                C = get_or_request_contact(A, BMF, BMT),
                db:save(#db_contact_roles{id=NPUID, type=db_task, role=wf:to_list(R), tid=UID, contact=C})
        end, Involved);
apply_message(Message, FID, ToID) ->
    error_logger:warning_msg("Wrong incomming message: ~p from ~p~n", [Message]).

get_vcard(BM, To, From) ->
    bitmessage:send_message(From, To, <<"Get vCard">>, BM, 6).
get_or_request_contact(BM, From, To) ->
    case db:get_contact_by_address(BM) of
        {ok, none} ->
            {ok, CID} = db:next_id(db_contact),
            db:save(#db_contact{id=CID, bitmessage=BM, address=BM}),
            get_vcard(BM, From, To),
            CID;
        {ok, Contact} ->
            #db_contact{id=CID} = Contact,
            CID
    end.

decode_attachments(A, BMF, BMT) ->
    AT = binary:split(A, <<";">>, [global, trim]),
    lists:map(fun(A) -> 
                    #db_file{id=FId, user=U} = F = binary_to_term(A),
                    C = get_or_request_contact(U, BMF, BMT),
                    NF = F#db_file{user=C, status=received},
                    db:save(NF),
                    FId
            end, AT).
