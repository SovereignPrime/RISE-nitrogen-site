-module(receiver).

-behaviour(gen_server).
-include_lib("bitmessage/include/bm.hrl").
-include("db.hrl").

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
apply_message(#message{from=BMF, to=BMT, subject= <<"Get vCard">>, text=Data, enc=6}, FID, ToID) ->
                       {ok,  #db_contact{name=Name, email=Email, phone=Phone, bitmessage=BM, address=Address} } = db:get_contact_by_address(Data),
                       bitmessage:send_message(BMT, BMF, <<"vCard">>, <<(wf:to_binary(Name))/bytes, ",", (wf:to_binary(Email))/bytes, ",", (wf:to_binary(Phone))/bytes, ",", (wf:to_binary(Address))/bytes, ",", (wf:to_binary(BM))/bytes>>, 6);
apply_message(#message{from=BMF, to=BMT, subject= <<"vCard">>, text=Data, enc=6}, FID, ToID) ->
    [Name, Email, Phone, Address, BM] = binary:split(Data, <<",">>, [global, trim]),
    {ok, Contact } = db:get_contact_by_address(BM),
    db:save(Contact#db_contact{name=Name, email=Email, phone=Phone, address=Address});

apply_message(#message{from=BMF, to=BMT, subject= <<"Get torrent">>, text=Data, enc=6}, FID, ToID) ->
    {ok,  F } = file:read_file("scratch/" ++ wf:to_list(Data)),
    bitmessage:send_message(BMT, BMF, <<"torrent">>, F);

apply_message(#message{from=BMF, to=BMT, subject= <<"torrent">>, text=Data, enc=6}, FID, ToID) ->
    [Id, Torrent] = binary:split(Data, <<";">>, [global, trim]),
    file:write_file(wf:f("scratch/~s.torrent", [Id]), Torrent);

apply_message(#message{from=BMF, to=BMT, subject=Subject, text=Data, enc=Enc}, FID, ToID) when Enc == 2; Enc == 3 ->
    {ok, Id} = db:next_id(db_update),
    {match, [_, <<Text/bytes>>, <<InvolvedB/bytes>>]} = re:run(Data, "^(.*)\nInvolved:(.*)$", 
                                                               [{capture, all, binary}, ungreedy, dotall, firstline, {newline, any}]),
    Involved = binary:split(InvolvedB, <<";">>, [global, trim]),
    Message = #db_update{id=Id, date=date(), from=FID, to=Involved -- [BMT], subject=wf:to_list(Subject), text=Text, status=unread},
    db:save(Message);
apply_message(#message{from=BMF, to=BMT, subject=Subject, text=Data, enc=Enc}, FID, ToID) when Enc == 2; Enc == 3 ->
    {ok, Id} = db:next_id(db_update),
    {match, [_, <<Text/bytes>>, <<InvolvedB/bytes>>, <<A/bytes>>]} = re:run(Data, "^(.*)\nInvolved:(.*)\nAttachments:(.*)$", 
                                                                 [{capture, all, binary}, ungreedy, dotall, firstline, {newline, any}]),
    Involved = binary:split(InvolvedB, <<";">>, [global, trim]),
    Message = #db_update{id=Id, date=date(), from=FID, to=Involved -- [BMT], subject=wf:to_list(Subject), text=Text, status=unread},
    db:save(Message),
    AT = binary:split(A, <<";">>, [global, trim]),
    Files = lists:map(fun(A) -> 
                    #db_file{id=Id, user=U} = F = binary_to_term(A),
                    C = get_or_request_contact(U, BMF, BMT),
                    NF = F#db_file{user=C},
                    db:save(NF),
                    Id
            end, AT),
    db:save_attachments(Message, Files);
apply_message(#message{from=BMF, to=BMT, subject=Subject, text=Data, enc=Enc}, FID, ToID) when Enc == 4; Enc == 5 ->
    {match, [_, <<Name/bytes>>, <<InvolvedB/bytes>>, <<Due/bytes>>, <<Status/bytes>>, UID]} = re:run(Data, "^(.*)\nInvolved:(.*)\nDue:(.*)\nStatus:(.*)\nUID:(.*)$", 
                                                                                                [{capture, all, binary}, ungreedy, dotall, firstline, {newline, any}]),
    Involved = binary:split(InvolvedB, <<";">>, [global, trim]),
    {ok, Id} = db:get_task_by_uid(UID),
    db:save(#db_task{id=Id, uid=UID, due=Due,  name=wf:to_list(Subject), text=Name, status=Status}),
    lists:foreach(fun(I) ->
                [BM, Role] = binary:split(I, <<":">>),
                {ok, NPId} = db:next_id(db_contact_roles),
                C = get_or_request_contact(BM, BMF, BMT),
                db:save(#db_contact_roles{id=NPId, type=db_task, role=Role, tid=Id, contact=C})
        end, Involved);
apply_message(Message, FID, ToID) ->
    error_logger:warning_img("Wrong incomming message: ~p from ~p~n", [Message]).

get_vcard(BM, To, From) ->
    bitmessage:send_message(From, To, <<"Get vCard">>, BM, 6).
get_or_request_contact(BM, From, To) ->
    case db:get_contact_by_address(BM) of
        {ok,  none } ->
            {ok, CID} = db:next_id(db_contact),
            db:save(#db_contact{id=CID, bitmessage=BM, address=BM}),
            get_vcard(BM, From, To),
            CID;
        {ok, Contact} ->
            #db_contact{id=CID} = Contact,
            CID
    end.
