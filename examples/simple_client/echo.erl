-module(echo).
-behaviour(teacup_server).

-export([start_link/2]).
-export([teacup@handle/2]).

-defined(DEFAULT_PORT, 19999).

%% == API

say(Host, What) when is_binary(What) ->
    {ok, Conn} = teacup:new(echo),
    ok = teacup:connect(Conn, "127.0.0.1"),
    {ok, Msg} = teacup:send_sync(Conn, What),
    ok = teacup:disconnect(Conn),
    Msg.

say(Host, What) ->
    say(Host, list_to_binary(What)).

%% == Callbacks

teacup@handle({init, Opts}) ->
    State = Opts#{connect => #{port => ?DEFAULT_PORT}},
    {ok, State};

teacup@connect(State) ->
    io:format("connect"),
    {noreply, State}.

teacup@data(Data, State#{socket := Socket}) ->
    io:format("received: ~p~n", [Data]),
    ok = gen_tcp:send(Socket, Data),
    {noreply, State}.

teacup@disconnect(State) ->
    ok.

%% == Internal

default_connection_options() ->
    #{port := ?DEFAULT_PORT}.