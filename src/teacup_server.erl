% Copyright (c) 2016, Yuce Tekol <yucetekol@gmail.com>.
% All rights reserved.

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:

% * Redistributions of source code must retain the above copyright
%   notice, this list of conditions and the following disclaimer.

% * Redistributions in binary form must reproduce the above copyright
%   notice, this list of conditions and the following disclaimer in the
%   documentation and/or other materials provided with the distribution.

% * The names of its contributors may not be used to endorse or promote
%   products derived from this software without specific prior written
%   permission.

% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(teacup_server).
-behaviour(gen_server).

-export([start_link/4,
         connect/1,
         connect/3,
         disconnect/1,
         send/2,
         call/2,
         cast/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).
-export([teacup@init/1,
         teacup@status/2,
         teacup@error/2,
         teacup@call/3,
         teacup@cast/2,
         teacup@info/2]).

-type state() :: map().
-type callback_return() :: {ok, NewState :: state()} |
                           {stop, NewState :: state()} |
                           {error, Reason :: term(), State :: state()}.
-type otp_callback_return() :: {reply, Reply :: term(), State :: state()} |
                               {noreply, State :: state()} |
                               {stop, Reason :: term(), State :: state()}.

-callback teacup@data(Data :: iodata(), State :: state()) ->
    callback_return().

-callback teacup@init(Opts :: map()) ->
    callback_return().

-callback teacup@status(Status :: atom(), State :: state()) ->
    callback_return().

-callback teacup@error(Reason :: term(), State :: state()) ->
    callback_return().

-callback teacup@call(Message :: term(), _From :: pid(), State :: state()) ->
    otp_callback_return().

-callback teacup@cast(Message :: term(), State :: state()) ->
    otp_callback_return().

-callback teacup@info(Message :: term(), State :: state()) ->
    otp_callback_return().

-optional_callbacks([teacup@init/1,
                     teacup@status/2,
                     teacup@error/2,
                     teacup@call/3,
                     teacup@cast/2,
                     teacup@info/2]).

%% == API

-spec start_link(Parent :: pid(), Ref :: teacup:teacup_ref(), Handler :: atom(), Opts :: map()) ->
    {ok, pid()} | ignore | {error, Reason :: term()}.
start_link(Parent, Ref, Handler, Opts) ->
    gen_server:start_link(?MODULE, [Parent, Ref, Handler, Opts], []).

-spec connect(Pid :: pid()) -> ok.
connect(Pid) ->
    gen_server:cast(Pid, connect).

-spec connect(Pid :: pid(), Host :: binary(), Port :: non_neg_integer()) ->
    ok.
connect(Pid, Host, Port) ->
    gen_server:cast(Pid, {connect, Host, Port}).

-spec disconnect(Pid :: pid()) -> ok.
disconnect(Pid) ->
    gen_server:cast(Pid, disconnect).

-spec send(Pid :: pid(), Data :: iodata()) -> ok.
send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

-spec call(Pid :: pid(), Msg :: term()) -> term().
call(Pid, Msg) ->
    gen_server:call(Pid, Msg).

-spec cast(Pid :: pid(), Msg :: term()) -> ok.    
cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

%% == Callbacks

init([Parent, Ref, Handler, Opts]) ->
    OptCallbacks = opt_callbacks(Handler),
    TInit = maps:get(teacup@init, OptCallbacks),
    case TInit(Opts) of
        {ok, State1} ->
            State2 = initial_state(State1),
            NewState = State2#{parent@ => Parent,
                               ref@ => Ref,
                               handler@ => Handler,
                               registered@ => false,
                               socket@ => undefined,
                               callbacks@ => OptCallbacks,
                               transport_pid@ => undefined},
            {ok, NewState, 0};
        {error, _} = Error ->
            Error
    end.

handle_call(Msg, From, State) ->
    handle_gen_call(Msg, From, State).

handle_cast(connect, State) ->
    connect_server(State);

handle_cast({connect, Host, Port}, State) ->
    connect_server(Host, Port, State);

handle_cast(disconnect, State) ->
    disconnect_server(State);

handle_cast({send, Data}, State) ->
    send_data(Data, State);
    
handle_cast(Msg, State) ->
    handle_gen_cast(Msg, State).

handle_info(timeout, #{registered@ := false,
                       ref@ := Ref} = State) ->
    teacup_registry:update(Ref, self()),
    NewState = State#{registered@ := true},
    {noreply, NewState};
    
handle_info(timeout, #{registered@ := true} = State) ->
    handle_gen_info(timeout, State);

handle_info({transport_data, Socket, Data}, #{socket@ := Socket} = State) ->
    handle_tcp_data(Data, State);

handle_info({transport_closed, Socket}, #{socket@ := Socket} = State) ->
    handle_tcp_closed(State);

handle_info(Msg, State) ->
    handle_gen_info(Msg, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #{ref@ := Ref} = State) ->
    _ = disconnect_server(State),
    teacup_registry:remove(Ref),
    ok.

%% == Internal

initial_state(State) ->
    TransportOpts = maps:get(transport, State, #{}),
    NewTransportOpts = maps:merge(default_transport_opts(), TransportOpts),
    State#{transport => NewTransportOpts}.

connect_server(#{transport := #{host := Host,
                              port := Port}} = State) ->
    connect_server(Host, Port, State).

connect_server(Host, Port,
               #{callbacks@ := #{teacup@error := TError},
                 transport := #{handler := TrHandler,
                                timeout := Timeout,
                                options := Options} = Transport} = State) ->
    NewTransport = maps:merge(Transport, #{host => Host,
                                       port => Port}),
    NewState = State#{transport => NewTransport},
    TrPid = spawn_link(TrHandler, loop, [self()]),
    case TrHandler:connect(binary_to_list(Host), Port, Options, Timeout) of
        {ok, Socket} ->
            gen_tcp:controlling_process(TrHandler:raw_socket(Socket), TrPid),
            handle_status(connect, NewState#{socket@ => Socket,
                                             transport_pid@ => TrPid});
        {error, Reason} ->
            handle_result(TError(Reason, NewState))
    end.

disconnect_server(#{socket@ := undefined} = State) ->
    {noreply, State};

disconnect_server(#{socket@ := Socket,
                    transport := #{handler := TrHandler},
                    transport_pid@ := TrPid} = State) ->
    TrHandler:close(Socket),
    TrPid ! stop,
    NewState = reset_transport(State),
    {noreply, NewState}.

send_data(Data, #{socket@ := Socket,
                  callbacks@ := #{teacup@error := TError},
                  transport := #{handler := TrHandler}} = State) ->
    case TrHandler:send(Socket, Data) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            handle_result(TError(Reason, State))
    end.

default_transport_opts() ->
    #{handler => tcp@teacup,
      host => undefined,
      port => undefined,
      timeout => 1000,
      options => [binary, {packet, 0}]}.

opt_callbacks(Handler) ->
    ExportedSet = teacup_utils:exported_functions(Handler),
    F = fun({Name, Arity} = FN) ->
        Mod = case sets:is_element(FN, ExportedSet) of
            true -> Handler;
            _ -> ?MODULE
        end,
        {Name, fun Mod:Name/Arity}
    end,
    Ls = [{teacup@init, 1}, {teacup@status, 2},
          {teacup@call, 3}, {teacup@cast, 2},
          {teacup@error, 2}, {teacup@info, 2}],
    maps:from_list(lists:map(F, Ls)).

handle_tcp_data(Data, #{handler@ := Handler} = State) ->
    handle_result(Handler:teacup@data(Data, State)).

handle_tcp_closed(#{callbacks@ := #{teacup@status := TStatus}} = State) ->
    case TStatus(disconnect, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {stop, NewState} ->
            NewState1 = reset_transport(NewState),
            {stop, normal, NewState1};
        {error, Reason} ->
            NewState = reset_transport(State),
            {stop, Reason, NewState}
    end.

handle_gen_call(Msg, From, #{callbacks@ := #{teacup@call := TCall}} = State) ->
    TCall(Msg, From, State).

handle_gen_cast(Msg, #{callbacks@ := #{teacup@cast := TCast}} = State) ->
    TCast(Msg, State).

handle_gen_info(Msg, #{callbacks@ := #{teacup@info := TInfo}} = State) ->
    TInfo(Msg, State).

handle_status(Status, #{callbacks@ := #{teacup@status := TStatus}} = State) ->
    handle_result(TStatus(Status, State)).

reset_transport(State) ->
    State#{socket@ => undefined,
           transport_pid@ => undefined}.

handle_result({ok, State}) ->
    {noreply, State};

handle_result({stop, State}) ->
    {_, NewState} = disconnect_server(State),
    {stop, normal, NewState};

handle_result({error, Reason, State}) ->
    {_, NewState} = disconnect_server(State),
    {stop, Reason, NewState}.

%% == Defaults for optional callbacks

teacup@init(Opts) ->
    {ok, Opts}.

teacup@status(disconnect, State) ->
    {ok, State};

teacup@status(_, State) ->
    {ok, State}.

teacup@error(Reason, State) ->
    {error, Reason, State}.

teacup@call(_Message, _From, State) ->
    {stop, not_supported, State}.
    
teacup@cast(_Message, State) ->
    {stop, not_supported, State}.

teacup@info(_Message, State) ->
    {noreply, State}.

%% == Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

connect_test() ->
    application:start(teacup),
    {ok, C} = teacup:new(proxy@teacup),
    teacup:connect(C, <<"httpbin.org">>, 80),
    receive
        {C, {teacup@status, connect}} -> ok
    after 1000 ->
        ?assertEqual(true, false)
    end,
    teacup:send(C, <<"GET / HTTP/1.0\r\n\r\n">>),
    receive
        {C, _Data} -> ok
    after 1000 ->
        ?assertEqual(true, false)
    end,
    application:start(teacup).

-endif.