-module(ishtar_http).

-export([init/3]).

-record(state, {transport,
                     socket,
                     header_timeout,
                     body_timeout,
                     max_headers_count,
                     max_header_size,
                     raw_opts,
                     handler}).

-include("ishtar.hrl").

init(Transport, Socket, Opts) ->
    HeaderTimeout = proplists:get_value(header_timeout, Opts, 5000),
    BodyTimeout = proplists:get_value(body_timeout, Opts, 5000),
    MaxHeadersCount = proplists:get_value(max_headers_count, Opts, 100),
    MaxHeaderSize = proplists:get_value(max_header_size, Opts, 8192),
    Handler = proplists:get_value(handler, Opts),

    wait_request(0, #state{transport=Transport,
                                 socket=Socket,
                                 header_timeout=HeaderTimeout,
                                 body_timeout=BodyTimeout,
                                 max_headers_count=MaxHeadersCount,
                                 max_header_size=MaxHeaderSize,
                                 raw_opts = Opts,
                                 handler = Handler}).


wait_request(NParsed, #state{transport=Transport, socket=Socket,
                                     header_timeout=HeaderTimeout}=State) ->

    case Transport:recv(Socket, 0, HeaderTimeout) of
        {ok, Data} ->
            parse_request(Data, NParsed, State);
        Error ->
            Error
    end.

parse_request(<<>>, NParsed, State) ->
    wait_request(NParsed, State);
parse_request(_, NParsed, #state{max_header_size=MaxLength}=State) when
        NParsed > MaxLength ->
    error_terminate(400, State);

parse_request(<< $\r, Rest/binary >>, NParsed, State) ->
    parse_request(Rest, NParsed+1, State);
parse_request(<< $\n, Rest/binary >>, NParsed, State) ->
    parse_request(Rest, NParsed+1, State);
parse_request(<< C, _/binary >>=Buf, NParsed, State) ->
    case ?IS_ALPHA(C) of
        true ->
            Method = case C of
                $C -> << "CONNECT" >>; %% or COPY, CHECKOUT
                $D -> << "DELETE" >>;
                $G -> << "GET" >>;
                $H -> << "HEAD" >>;
                $L -> << "LOCK" >>;
                $M -> << "MKCOL" >>; %% or MOVE, MKACTIVITY, MERGE, M-SEARCH
                $N -> << "NOTIFY" >>;
                $P -> << "POST" >>; %% or PROPFIND|PROPPATCH|PUT|PATCH|PURGE
                $R -> << "REPORT" >>;
                $S -> << "SUBSCRIBE" >>; %% or SEARCH
                $T -> << "TRACE" >>;
                $U -> << "UNLOCK" >>; %% or UNSUBSRIBE
                _ ->
                    error
            end,

            case Method of
                error ->
                    error_terminate(400, State);
                _ ->
                    parse_method(Method, Buf, NParsed+1, << C >>, 0, State)
            end;
        _ ->
            error_terminate(400, State)
    end;
parse_request(_, _, State) ->
    error_terminate(400, State).

parse_method(M, <<>>, NParsed, Got, Idx,
             #state{transport=Transport, socket=Socket,
                    header_timeout=Timeout}=State) ->
    recv(Transport, Socket, Timeout, fun(Data) ->
                parse_method(M, Data, NParsed, Got, Idx, State)
        end);

parse_method(<<"CONNECT">> = M, << C, Rest/binary >>, NParsed, Got, Idx,
             State) ->
    case C of
        $H when Idx =:= 0 ->

            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"CHECKOUT">>, State);
        $P when Idx =:= 1 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"COPY">>, State);
        $O when Idx =:= 0 ->
            parse_method(M, Rest, NParsed+1, << Got/binary, C >>, Idx+1,
                         State);
        _ ->
            error_terminate(400, State)
    end;

parse_method(<<"MKCOL">>=M, << C, Rest/binary >>, NParsed, Got, Idx,
             State) ->
    case C of
        $O when Idx =:= 0 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"MOVE">>, State);
        $E when Idx =:= 0 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"MERGE">>, State);
        $- when Idx =:= 0 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"MSEARCH">>, State);
        $A when Idx =:= 1 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"MKACTIVITY">>, State);

        $K when Idx =:= 0 ->
            parse_method(M, Rest, NParsed+1, << Got/binary, C >>, Idx+1,
                         State);
        _ ->
            error_terminate(400, State)
    end;
parse_method(<<"SUBSCRIBE">>=M, << C, Rest/binary >>, NParsed, Got, Idx,
             State) ->
    case C of
        $E when Idx =:= 0 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"SEARCH">>, State);
        $U when Idx =:= 0 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          M, State);
        _ ->
            error_terminate(400, State)
    end;

parse_method(<<"POST">>=M, << C, Rest/binary >>, NParsed, Got, Idx,
             State) ->
    case C of
        $R when Idx =:= 0 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"PROFIND">>, State);
        $A when Idx =:= 0 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"PATCH">>, State);
        $O when Idx =:= 0 ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          M, State);
        $U when Idx =:= 0 ->
            parse_method(<<"PUT">>, Rest, NParsed+1, << Got/binary, C >>, Idx+1,
                         State);
        _ ->
            error_terminate(400, State)
    end;
parse_method(<<"PUT">>=M, << C, Rest/binary >>, NParsed, Got, 1,
             State) ->
    case C of
        $R ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"PURGE">>, State);
        _ ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          M, State)
    end;
parse_method(<<"UNLOCK">>=M, << C, Rest/binary >>, NParsed, Got, 0,
             State) ->
    case C of
        $N ->
            parse_method(M, Rest, NParsed+1, << Got/binary, C >>, 1,
                         State);
        _ ->
            error_terminate(400, State)
    end;
parse_method(<<"UNLOCK">>=M, << C, Rest/binary >>, NParsed, Got, 1,
             State) ->
    case C of
        $S ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          <<"UNSUBSCRIBE">>, State);
        $L ->
            parse_method1(Rest, NParsed+1, << Got/binary, C >>,
                          M, State);
        _ ->
            error_terminate(400, State)
    end;
parse_method(_, _, _, _, _, State) ->
    error_terminate(400, State).


parse_method1(<<>>, NParsed, Got, Method,
              #state{transport=Transport, socket=Socket,
                     header_timeout=Timeout}=State) ->
    recv(Transport, Socket, Timeout, fun(Data) ->
                parse_method1(Data, NParsed, Got, Method, State)
        end);
parse_method1(_, NParsed, _, _, #state{max_header_size=MaxLength}=State)
        when NParsed > MaxLength->
    error_terminate(414, State);
parse_method1(<< $\s, Rest/binary >>, NParsed, Method, Method, State) ->
    parse_url(Rest, NParsed, State, Method);
parse_method1(<< C, Rest/binary >>, NParsed, Got, Method, State) ->
    case ?IS_ALPHA(C) of
        true ->
            parse_method1(Rest, NParsed, << Got/binary, C >>, Method, State);
        _ ->
            error_terminate(400, State)
    end.

parse_url(_Buffer, _NParsed, _State, _Method) ->
    ok.


recv(Transport, Socket, Timeout, Fun) ->
    case Transport:recv(Socket, 0, Timeout) of
        {ok, Data} ->
            Fun(Data);
        Error ->
            Error
    end.

error_terminate(_Code, _state) ->
    ok.
