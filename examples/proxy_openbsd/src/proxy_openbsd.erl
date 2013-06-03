-module(proxy_openbsd).

-export([start/0]).
-export([proxy/1]).


start() ->
    ok = application:start(lager),
    ok = application:start(ishtar),
    ok = application:start(proxy_openbsd).

proxy(_Data) ->
    {remote, {"www.openbsd.org", 80}}.
