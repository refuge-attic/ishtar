-module(ishtar_listener).
-behaviour(gen_server).

-export([start_listener/6, start_listener/7,
         stop_listener/1,
         get_port/1]).


-export([start_link/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


-record(state, {socket,
                transport,
                transport_opts,
                acceptors,
                open_reqs,
                listener_opts,
                protocol}).


start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtocolOpts) ->
        start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
                       ProtocolOpts, []).

start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtoOpts, ListenerOpts0) ->
    _ = code:ensure_loaded(Transport),
    case erlang:function_exported(Transport, name, 0) of
		false ->
			{error, badarg};
		true ->
            ListenerOpts = [{ref, Ref} | ListenerOpts0],
            supervisor:start_child(ishtar_sup,
                                   child_spec(Ref, [NbAcceptors, Transport,
                                                     TransOpts, Protocol,
                                                     ProtoOpts,
                                                     ListenerOpts]))
    end.

stop_listener(NameOrPid) ->
    case supervisor:terminate_child(ishtar_sup, NameOrPid) of
        ok ->
            supervisor:delete_child(ishtar_sup, NameOrPid);
        Error ->
            Error
    end.


get_port(NameOrPid) ->
    gen_server:call(NameOrPid, get_port).

%% @doc return a child spec suitable for embeding your listener in the
%% supervisor
child_spec(Ref, Options) ->
    {Ref, {ishtar_listener, start_link, [Options]},
            permanent, 5000, worker, [Ref]}.



start_link(Options) ->
    case proplists:get_value(name, Options) of
        undefined ->
            gen_server:start_link(?MODULE, Options, []);
        Name ->
            gen_server:start_link({local, Name}, ?MODULE, Options, [])
    end.

init([NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts,
      ListenerOpts]) ->

    process_flag(trap_exit, true),

    {ok, Socket} = Transport:listen(TransOpts),

    %% launch acceptors
    Acceptors = [ishtar_acceptor:start_link(self(), Transport, Socket,
                                            ListenerOpts,
                                            {Protocol, ProtoOpts})
                 || _ <- lists:seq(1, NbAcceptors)],
    {ok, #state{socket = Socket,
                transport = Transport,
                transport_opts = TransOpts,
                acceptors = Acceptors,
                open_reqs = 0,
                listener_opts = ListenerOpts,
                protocol = {Protocol, ProtoOpts}}}.


handle_call(get_port, _From, #state{socket=S, transport=Transport}=State) ->
    case Transport:peername(S) of
        {ok, {_, Port}} ->
            {reply, {ok, Port}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(accepted, State) ->
    NewState = start_new_acceptor(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, {error, emfile}}, State) ->
    lager:error("No more file descriptors, shutting down~n", []),
    {stop, emfile, State};

handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, remove_acceptor(State, Pid)};

handle_info({'EXIT', Pid, Reason}, State) ->
    lager:error("request (pid ~p) unexpectedly crashed:~n~p~n",
                [Pid, Reason]),
    {noreply, remove_acceptor(State, Pid)}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


remove_acceptor(State, Pid) ->
    State#state{acceptors = lists:delete(Pid, State#state.acceptors),
                open_reqs = State#state.open_reqs - 1}.

start_new_acceptor(State) ->
    Pid = ishtar_acceptor:start_link(self(), State#state.transport,
                                     State#state.socket,
                                     State#state.listener_opts,
                                     State#state.protocol),

    State#state{acceptors = [Pid | State#state.acceptors],
                open_reqs = State#state.open_reqs + 1}.
