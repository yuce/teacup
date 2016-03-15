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
         send/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).
-export([teacup@init/1,
         teacup@status/2,
         teacup@error/2,
         teacup@message/2]).

-type state() :: map().
-type callback_return() :: {ok, NewState :: map()} |
                           {stop, NewState :: map()} |
                           {reconnect, Newstate :: map()} |
                           {error, Reason :: term()}.


% -callback teacup@handle({init, Opts :: map()} |
%                         {data, Data :: iodata()} |
%                         {message, Message :: term()} |
%                         {error, Reason :: term()} |
%                         connect | disconnect, State :: state()) ->
%     {ok, NewState :: map()}
%     | {stop, NewState :: map()}
%     | {error, Reason :: term()}.


-callback teacup@init(Opts :: map()) ->
    callback_return().

-callback teacup@status(Status :: atom(), State :: state()) ->
    callback_return().

-callback teacup@error(Reason :: term(), State :: state()) ->
    callback_return().

-callback teacup@message(Message :: term(), State :: state()) ->
    callback_return().

-callback teacup@data(Data :: iodata(), State :: state()) ->
    callback_return().

-optional_callbacks([teacup@init/1,
                     teacup@status/2,
                     teacup@error/2,
                     teacup@message/2]).

%% == API

start_link(Parent, Ref, Handler, Opts) ->
    gen_server:start_link(?MODULE, [Parent, Ref, Handler, Opts], []).

connect(Pid) ->
    gen_server:cast(Pid, connect).

connect(Pid, Host, Port) ->
    gen_server:cast(Pid, {connect, Host, Port}).

disconnect(Pid) ->
    gen_server:cast(Pid, disconnect).

send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

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
                               callbacks@ => OptCallbacks},
            % case check_initial_state(NewState) of
            %     ok -> {ok, NewState, 0};
            %     Error ->
            %         Error
            % end;
            {ok, NewState, 0};
        {error, _} = Error ->
            Error
    end.

handle_call(_, _From, State) ->
    {stop, not_allowed, State}.


handle_cast(connect, State) ->
    connect_server(State);

handle_cast({connect, Host, Port}, State) ->
    connect_server(Host, Port, State);

handle_cast(disconnect, State) ->
    disconnect_server(State);

handle_cast({send, Data}, State) ->
    send_data(Data, State).

handle_info(timeout, #{registered@ := false,
                       ref@ := Ref} = State) ->
    teacup_registry:update(Ref, self()),
    NewState = State#{registered@ := true},
    {noreply, NewState};

handle_info({tcp, Socket, Data}, #{socket@ := Socket} = State) ->
    handle_tcp_data(Data, State);

handle_info({tcp_closed, Socket}, #{socket@ := Socket} = State) ->
    handle_tcp_closed(State);

handle_info(Msg, State) ->
    handle_message(Msg, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #{ref@ := Ref} = State) ->
    _ = disconnect_server(State),
    teacup_registry:remove(Ref),
    ok.

%% == Internal

initial_state(State) ->
    ConnectOpts = maps:get(connect, State, #{}),
    NewConnectOpts = maps:merge(default_connect_opts(), ConnectOpts),
    State#{connect => NewConnectOpts}.

connect_server(#{connect := #{host := Host,
                              port := Port}} = State) ->
    connect_server(Host, Port, State).

connect_server(Host, Port, #{callbacks@ := #{teacup@error := TError},
                             connect := #{timeout := Timeout,
                                          options := Options} = Connect} = State) ->
    NewConnect = maps:merge(Connect, #{host => Host,
                                       port => Port}),
    NewState = State#{connect => NewConnect},
    case gen_tcp:connect(Host, Port, Options, Timeout) of
        {ok, Socket} ->
            handle_status(connect, NewState#{socket@ => Socket});
        {error, Reason} ->
            handle_result(TError(Reason, State), NewState)
    end.

disconnect_server(#{socket@ := undefined} = State) ->
    {noreply, State};

disconnect_server(#{socket@ := Socket} = State) ->
    gen_tcp:close(Socket),
    NewState = reset_socket(State),
    {noreply, NewState}.

send_data(Data, #{socket@ := Socket,
                  callbacks@ := #{teacup@error := TError}} = State) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            handle_result(TError(Reason, State), State)
    end.

default_connect_opts() ->
    #{host => undefined,
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
          {teacup@error, 2}, {teacup@message, 2}],
    maps:from_list(lists:map(F, Ls)).

handle_tcp_data(Data, #{handler@ := Handler} = State) ->
    handle_result(Handler:teacup@data(Data, State), State).

handle_tcp_closed(#{callbacks@ := #{teacup@status := TStatus}} = State) ->
    case TStatus(disconnect, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {reconnect, NewState} ->
            connect_server(NewState);
        {stop, NewState} ->
            NewState1 = reset_socket(NewState),
            {stop, normal, NewState1};
        {error, Reason} ->
            NewState = reset_socket(State),
            {stop, Reason, NewState}
    end.

handle_message(Msg, #{callbacks@ := #{teacup@message := TMessage}} = State) ->
    handle_result(TMessage(Msg, State), State).

handle_status(Status, #{callbacks@ := #{teacup@status := TStatus}} = State) ->
    handle_result(TStatus(Status, State), State).

reset_socket(State) ->
    State#{socket@ => undefined}.

handle_result({ok, NewState}, _State) ->
    {noreply, NewState};

handle_result({stop, NewState}, State) ->
    {_, NewState} = disconnect_server(State),
    {stop, normal, NewState};

handle_result({error, Reason}, State) ->
    {_, NewState} = disconnect_server(State),
    {stop, Reason, NewState}.

%% == Defaults for optional callbacks

teacup@init(Opts) ->
    {ok, Opts}.

teacup@status(disconnect, State) ->
    {ok, State};

teacup@status(_, State) ->
    {ok, State}.

teacup@error(Reason, _State) ->
    {error, Reason}.

teacup@message(_Message, State) ->
    {ok, State}.
