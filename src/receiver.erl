-module(receiver).

-behaviour(bitmessage).
-behaviour(gen_server).
-include_lib("bitmessage/include/bm.hrl").
-include("db.hrl").
-include("protokol.hrl").

%% API
-export([  % {{{1
    start_link/0,
    register_receiver/1,
    received/1,
    sent/1,
    key_ready/1,
    connected/1,
    disconnected/1,
    downloaded/1,
    filechunk_sent/2,
    filechunk_received/2,
	extract_task/1
    ]). % }}}

%% gen_server callbacks
-export([init/1,  % {{{1
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).  % }}}

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
start_link() ->  % {{{1
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_receiver(Pid) ->  % {{{1
    gen_server:cast(?MODULE, {register, Pid}).

-spec received(binary()) -> ok. % {{{1
received(Hash) ->
    gen_server:cast(?MODULE, {msg, Hash}).

-spec sent(binary()) -> ok. % {{{1
sent(Hash) ->
    gen_server:cast(?MODULE, {sent, Hash}).

-spec key_ready(binary()) -> ok.  % {{{1
key_ready(Address) ->
    gen_server:cast(?MODULE, {address, Address}).

-spec connected(non_neg_integer()) -> ok.  % {{{1
connected(N) ->
    gen_server:cast(?MODULE, {connection, N}).

-spec disconnected(non_neg_integer()) -> ok.  % {{{1
disconnected(N) ->
    gen_server:cast(?MODULE, {connection, N}).

-spec downloaded(binary()) -> ok. % {{{1
downloaded(Hash) ->
    gen_server:cast(?MODULE, {downloaded, Hash}).
-spec filechunk_received(binary(), binary()) -> ok. % {{{1
filechunk_received(FileHash, ChunkHash) ->
    gen_server:cast(?MODULE, {filechunk_received, FileHash, ChunkHash}).
-spec filechunk_sent(binary(), binary()) -> ok. % {{{1
filechunk_sent(FileHash, ChunkHash) ->
    gen_server:cast(?MODULE, {filechunk_sent, FileHash, ChunkHash}).

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
init([]) ->  % {{{1
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
handle_call(_Request, _From, State) ->  % {{{1
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
handle_cast({address, Address}, #state{pid=Pid}=State) ->  % {{{1
    {ok, U} = db:create_account("", true, Address),
    Pid ! accepted,
    {noreply, State};
handle_cast({register, Pid}, State) ->  % {{{1
    {noreply, State#state{pid=Pid}};
handle_cast({connection, N}, State) ->  % {{{1
    State#state.pid ! {status, N},
    {noreply, State};
handle_cast({downloaded, Hash}, State) ->  % {{{1
    [#bm_file{name=Name}] = bm_db:lookup(bm_file, Hash),
    db:mark_downloaded(Name),
    {noreply, State};
handle_cast({sent, Hash}, State) ->  % {{{1
    {ok, #message{enc=Enc}}= bitmessage:get_message(Hash),
    case Enc of
        E when E==2; 
               E == 3;
               E == 4 ->
            State#state.pid ! sent,
            {noreply, State};
        _ ->
            {noreply, State}
    end;
handle_cast({msg, Hash}, State) ->  % {{{1
    io:format("Receiver: ~p~n", [Hash]),
    {ok, #message{from=From,
                  to=To,
                  subject=Subject,
                  text=Text,
                  enc=Enc} = Message}= bitmessage:get_message(Hash),

    FID = get_or_request_contact(From, From, To),
    {ok, #db_contact{id=ToID}} = db:get_contact_by_address(To),
    apply_message(Message, FID, ToID, State),
    State#state.pid ! update,
    {noreply, State};
handle_cast(_Msg, State) ->  % {{{1
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
handle_info(_Info, State) ->  % {{{1
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
terminate(_Reason, _State) ->  % {{{1
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->  % {{{1
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%
%%% Technical messages (not visible for users  {{{1
%%%

% Get vCard % {{{1
apply_message(#message{from=BMF,
                       to=BMT,
                       subject= <<"Get vCard">>,
                       text=Data,
                       enc=6}, FID, ToID, State) ->
                       {ok,  #db_contact{name=Name,
                                         email=Email,
                                         phone=Phone,
                                         bitmessage=BM,
                                         address=Address} } = db:get_contact_by_address(Data),

                       bitmessage:send_message(BMT,
                                               BMF,
                                               <<"vCard">>,
                                               <<(wf:to_binary(Name))/bytes,
                                                 ",",
                                                 (wf:to_binary(Email))/bytes,
                                                 ",",
                                                 (wf:to_binary(Phone))/bytes,
                                                 ",",
                                                 (wf:to_binary(Address))/bytes,
                                                 ",",
                                                 (wf:to_binary(BM))/bytes>>,
                                               6);

% vCard  {{{1
apply_message(#message{from=BMF,
                       to=BMT,
                       subject= <<"vCard">>,
                       text=Data,
                       enc=6}, FID, ToID, State) ->
    [Name, Email, Phone, Address, BM] = binary:split(Data, <<",">>, [global, trim]),
    {ok, Contact } = db:get_contact_by_address(BM),
    db:save(Contact#db_contact{name=wf:to_list(Name),
                               email=wf:to_list(Email),
                               phone=wf:to_list(Phone),
                               address=Address});
% Get torrent {{{1
apply_message(#message{from=BMF,
                       to=BMT,
                       subject= <<"Get torrent">>,
                       text=Data,
                       enc=6}, FID, ToID, State) ->
    {ok, FD} = application:get_env(etorrent_core, dir),
    {ok,  F } = file:read_file(FD ++ "/" ++ wf:to_list(Data) ++ ".torrent"),
    bitmessage:send_message(BMT, BMF, <<"torrent">>, <<Data/bytes, ";", F/bytes>>, 6);

% Torrent {{{1
apply_message(#message{from=BMF,
                       to=BMT,
                       subject= <<"torrent">>,
                       text=Data,
                       enc=6}, FID, ToID, State) ->
    [Id, Torrent] = binary:split(Data, <<";">>, [trim]),
    {ok, FD} = application:get_env(etorrent_core, dir),
    Path = wf:f("~s/~s.torrent", [FD, Id]),
    
    file:write_file(Path, Torrent);

% Task tree  {{{1
apply_message(#message{from=BMF,
                       to=BMT,
                       subject= <<"Task tree">>,
                       text=Data,
                       enc=6}, FID, ToID, State) ->
    #task_tree_packet{task=Task, parent=Parent, time=TS} = binary_to_term(Data),
    db:save_subtask(Task, Parent, TS);

% Update  {{{1
apply_message(#message{from=BMF,
                       to=BMT,
                       subject= <<"Update223322">>,
                       text=Data,
                       enc=2}, FID, ToID, State) ->
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
% }}}

%%%
%% Informational messages  {{{1
%%%

% Message {{{1
apply_message(#message{from=BMF,
                       to=BMT,
                       subject=Subject,
                       text=Data,
                       attachments=Attachments,
                       enc=3},
              FID,
              ToID,
              State)  ->
    error_logger:info_msg("Message from ~s received subject ~s~n", [BMF, Subject]),
    {ok, Id} = db:next_id(db_update),

    #message_packet{text=Text,
                    involved=Involved} = binary_to_term(Data),

    Message = #db_update{id=Id,
                         date=date(),
                         from=FID,
                         to=Involved -- [BMT],
                         subject=wf:to_list(Subject),
                         text=Text,
                         status=unread},

    db:save(Message),
    save_attachments(FID, Message, Attachments),
    State#state.pid ! received;

% Task  {{{1
apply_message(#message{from=BMF,
                       to=BMT,
                       subject=Subject,
                       attachments=Attachments, 
                       text=Data,
                       enc=4},
              FID,
              ToID,
              State)  ->

    #task_packet{id=UID, 
                 due=Due, 
                 %name=wf:to_list(Subject), 
                 text=Text, 
                 parent=Parent, 
                 status=Status, 
                 time=Time,
				 changes=Changes,
                 involved=Involved} = extract_task(Data),

    Task = case db:get_task(Parent) of
               {ok, []} ->
                   {ok, P} = db:search_parent(UID, Parent),
                   #db_task{id=UID,
                            due=Due,
                            name=wf:to_list(Subject),
                            text=Text,
                            parent=P,
                            status=Status,
						    changes=Changes};
               {ok, _} ->
                   #db_task{id=UID,
                            due=Due,
                            name=wf:to_list(Subject),
                            text=Text,
                            parent=Parent,
                            status=Status,
						    changes=Changes}
           end,
    db:save(Task),
    db:clear_roles(db_task, UID),
    save_attachments(FID, Task, Attachments),
    lists:foreach(fun(#role_packet{address=A, role=R}) ->
                {ok, NPUID} = db:next_id(db_contact_roles),
                C = get_or_request_contact(A, BMF, BMT),
                db:save(#db_contact_roles{id=NPUID,
                                          type=db_task,
                                          role=wf:to_list(R),
                                          tid=UID,
                                          contact=C})
        end, Involved),
    {ok, Children} = db:get_children(UID, Time),
    lists:foreach(fun(#db_task_tree{task=T, time=TS}) ->
                          db:save_subtask(T, UID, TS)
                  end, Children),
    State#state.pid ! received;

% Default  {{{1
apply_message(Message, FID, ToID, State) ->
    State#state.pid ! received,
    error_logger:warning_msg("Wrong incomming message: ~p from ~p~n", [Message, FID]).

get_vcard(BM, To, From) ->  % {{{1
    bitmessage:send_message(From, To, <<"Get vCard">>, BM, 6).
get_or_request_contact(BM, From, To) ->  % {{{1
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

extract_task(Data) when is_binary(Data) ->  % {{{1
	extract_task(binary_to_term(Data));
extract_task(Task) ->  % {{{1
    case Task of
        T = #task_packet{} -> T;
        {task_packet,
         Id,
         Name,
         Due,
         Text,
         Parent,
         Status,
         Involved,
         Attachments,
         Time} ->
            #task_packet{id=Id,
                         name=Name,
                         due=Due,
                         text=Text,
                         parent=Parent,
                         status=Status,
                         involved=Involved,
                         attachments=Attachments,
                         time=Time,
                         changes=[]}
	end.

-spec save_attachments(non_neg_integer(), record(), [#bm_file{}]) -> {ok, ok}.  % {{{1
save_attachments(UID, Message, Attachments) ->
    Files = lists:map(fun(#bm_file{
                             hash=I,
                             name=Name,
                             time={Date, _},
                             size=Size
                            }) ->
                                    db:save(#db_file{
                                              id=I,
                                              type=filename:extension(Name),
                                              path=Name,
                                              size=Size,
                                              date=Date,
                                              user=UID,
                                              status=received
                                             }),
                                    I
                      end,
                      Attachments),
    db:save_attachments(Message, sets:from_list(Files)).
