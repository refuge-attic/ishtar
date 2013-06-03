-module(ishtar).

-export([start/0, stop/0]).
-export([start_listener/6, start_listener/7,
         stop_listener/1]).


% --- Application ---

%% @doc Start the ishtar application. Useful when testing using the shell.
start() ->
    ishtar_deps:ensure(),
    application:load(ishtar),
    ishtar_app:ensure_deps_started(),
    application:start(ishtar).

%% @doc Start the coffer application. Useful when testing using the shell.
stop() ->
    application:stop(ishtar).


% --- ishtar API ---


start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtocolOpts) ->
        start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
                       ProtocolOpts, []).

start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtoOpts, ListenerOpts) ->
    ishtar_listener:start_listener(Ref, NbAcceptors, Transport, TransOpts,
                                    Protocol, ProtoOpts, ListenerOpts).

stop_listener(Ref) ->
    ishtar_listener:stop_listener(Ref).
