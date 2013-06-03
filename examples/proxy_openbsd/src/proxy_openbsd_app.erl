-module(proxy_openbsd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = ishtar:start_listener(http, 100, ishtar_tcp,
                                    [{port, 8080}], ishtar_revproxy,
                                    [{proxy, {proxy_openbsd, proxy}}]),

    proxy_openbsd_sup:start_link().

stop(_State) ->
    ok.
