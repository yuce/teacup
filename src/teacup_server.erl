-module(teacup_server).
-behaviour(gen_server).

-export([start_link/4,
         connect/2,
         connect/3,
         disconnect/1,
         send/2]).
-export([init/1,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-type state() :: map().
-type callback_return() :: {ok, NewState :: map()} |
                           {stop, NewState :: map()} |
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

connect(Pid, Host) ->
    gen_server:cast(Pid, {connect, Host}).

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
            case check_initial_state(NewState) of
                ok -> {ok, NewState, 0};
                Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

handle_cast(connect, State) ->
    connect_server(State);

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

handle_info({tcp_closed, Socket}, State) ->
    handle_tcp_closed(State);

handle_info(Msg, State) ->
    handle_message(Msg, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    _ = disconnect_server(State),
    ok.

%% == Internal

initial_state(State) ->
    ConnectOpts = maps:get(connect, State, #{}),
    NewConnectOpts = maps:merge(default_connect_opts(), ConnectOpts),
    State#{connect := NewConnectOpts}.

connect_server(#{callbacks@ := #{teacup@error := TError},
                                 connect := #{host := Host,
                                 port := Port,
                                 timeout := Timeout,
                                 options := Options}} = State) ->
    case gen_tcp:connect(Host, Port, Options, Timeout) of
        {ok, Socket} ->
            NewState = State#{socket@ => Socket},
            handle_status(connect, NewState);
        {error, Reason} ->
            handle_result(TError(Reason, State), State)
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

check_initial_state(#{connect := #{host := Host,
                                   port := Port}})
        when is_list(Host),
             is_integer(Port) ->
    ok;

check_initial_state(_) ->
    {stop, server_config}.

default_connect_opts() ->
    #{host => undefined,
      port => undefined,
      timeout => 1000,
      options => [binary, {packet, 0}]}.

% default_opts() ->
%     #{connect => default_connect_opts()}.

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
    {stop, State};

teacup@status(_, State) ->
    {ok, State}.

teacup@error(Reason, _State) ->
    {error, Reason}.

teacup@message(_Message, State) ->
    {ok, State}.


