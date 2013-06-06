-module(ishtar_util).

-export([filter_props/2, filter_props/3,
         propmerge/2,
         fix_ip/1,
         ipv6_supported/0,
         require/1,
         to_lower/1,
         to_upper/1]).

%% @doc filter a list of properties and removed n
filter_props(Props, Allowed) ->
    filter_props(Props, Allowed, []).

filter_props([], _Allowed, Acc) ->
    lists:reverse(Acc);
filter_props([{K, _}=KV|Rest], Allowed, Acc) ->
    case lists:member(K, Allowed) of
        true ->
            filter_props(Rest, Allowed, [KV|Acc]);
        false ->
            filter_props(Rest, Allowed, Acc)
    end.


%% @doc Update a proplist with values of the second. In case the same
%% key is in 2 proplists, the value from the first are kept.
propmerge(L1, L2) ->
    propmerge1(fun(_, V1, _) -> V1 end, L1, L2).

propmerge1(F, L1, L2) ->
	dict:to_list(dict:merge(F, dict:from_list(L1), dict:from_list(L2))).


fix_ip(Opts) ->
    {Opts1, ParsedIp} = case proplists:get_value(ip, Opts) of
        undefined ->
            {[{ip, any}|Opts], any};
        any ->
            {Opts, any};
        Ip when is_tuple(Ip) ->
            {Opts, Ip};
        Ip when is_list(Ip) ->
            {ok, IpTuple} = inet_parse:address(Ip),
            {[{ip, IpTuple}|proplists:delete(ip, Opts)], IpTuple}
    end,

    case ParsedIp of
        any ->
            case ipv6_supported() of % IPv4, and IPv6 if supported
                true -> [inet, inet6 | Opts1];
                _ -> Opts1
            end;
        {_, _, _, _} -> % IPv4
            [inet | Opts1];
        {_, _, _, _, _, _, _, _} -> % IPv6
            [inet6 | Opts1]
    end.

ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} ->
            true;
        {error, _} ->
            false
    end.


%% @doc Start the given applications if they were not already started.
-spec require(list(module())) -> ok.
require([]) ->
	ok;
require([App|Rest]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Rest).

%% @doc Convert a binary string to lowercase.
-spec to_lower(binary()) -> binary().
to_lower(L) ->
	<< << (char_to_lower(C)) >> || << C >> <= L >>.

to_upper(U) ->
    << << (char_to_upper(C)) >> || << C >> <= U >>.


%% @doc Convert [A-Z] characters to lowercase.
%% @end
%% We gain noticeable speed by matching each value directly.
-spec char_to_lower(char()) -> char().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.

%% @doc Convert [a-z] characters to uppercase.
-spec char_to_upper(char()) -> char().
char_to_upper($a) -> $A;
char_to_upper($b) -> $B;
char_to_upper($c) -> $C;
char_to_upper($d) -> $D;
char_to_upper($e) -> $E;
char_to_upper($f) -> $F;
char_to_upper($g) -> $G;
char_to_upper($h) -> $H;
char_to_upper($i) -> $I;
char_to_upper($j) -> $J;
char_to_upper($k) -> $K;
char_to_upper($l) -> $L;
char_to_upper($m) -> $M;
char_to_upper($n) -> $N;
char_to_upper($o) -> $O;
char_to_upper($p) -> $P;
char_to_upper($q) -> $Q;
char_to_upper($r) -> $R;
char_to_upper($s) -> $S;
char_to_upper($t) -> $T;
char_to_upper($u) -> $U;
char_to_upper($v) -> $V;
char_to_upper($w) -> $W;
char_to_upper($x) -> $X;
char_to_upper($y) -> $Y;
char_to_upper($z) -> $Z;
char_to_upper(Ch) -> Ch.
