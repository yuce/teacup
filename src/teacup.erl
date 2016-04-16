% Copyright 2016 Yuce Tekol <yucetekol@gmail.com>

% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at

%     http://www.apache.org/licenses/LICENSE-2.0

% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(teacup).

-export([new/1,
         new/2,
         connect/1,
         connect/3,
         disconnect/1,
         send/2,
         give/2,
         call/2,
         cast/2]).
-export([pid@/1]).

-define(REF(Handler, Ref), {teacup@ref, Handler, Ref}).

-type teacup_ref() :: {teacup@ref, Handler :: atom(), reference()}.
-export_type([teacup_ref/0]).

%% == API

new(Handler) ->
    new(Handler, #{}).

-spec new(atom(), map()) -> {ok, teacup_ref()} | {error, term()}.
new(Handler, Opts) ->
    Parent = self(),
    try signature(Handler, Opts) of
        Signature ->        
            TRef = ?REF(Signature, make_ref()),
            case teacup_server_sup:start_child(Parent, TRef, Handler, Opts) of
                {ok, _Pid} -> {ok, TRef};
                Other -> Other
            end
    catch
        {signature_error, E} ->
            {error, E}
    end.

-spec connect(TRef :: teacup_ref()) -> ok.
connect(TRef) ->
    run_ref(fun(P) -> teacup_server:connect(P) end, TRef).

-spec connect(TRef :: teacup_ref(), Host :: binary(), Port :: integer()) ->
    ok.
connect(TRef, Host, Port) ->
    run_ref(fun(P) -> teacup_server:connect(P, Host, Port) end, TRef).

-spec disconnect(TRef :: teacup_ref()) -> ok.
disconnect(TRef) ->
    run_ref(fun(P) -> teacup_server:disconnect(P) end, TRef).

-spec send(TRef :: teacup_ref(), What :: binary()) -> ok.
send(TRef, What) ->
    run_ref(fun(P) -> teacup_server:send(P, What) end, TRef).

-spec give(TRef :: teacup_ref(), NewParent :: pid()) -> ok.
give(TRef, NewParent) ->
    run_ref(fun(P) -> teacup_server:give(P, NewParent) end, TRef).

-spec call(TRef :: teacup_ref(), Msg :: term()) -> term().
call(TRef, Msg) ->
    run_ref(fun(P) -> teacup_server:call(P, Msg) end, TRef).

-spec cast(TRef :: teacup_ref(), Msg :: term()) -> ok.    
cast(TRef, Msg) ->
    run_ref(fun(P) -> teacup_server:cast(P, Msg) end, TRef).

-spec pid@(TRef :: teacup_ref()) -> not_found | {ok, pid()}.
pid@(TRef) ->
    teacup_registry:lookup(TRef).

%% == Internal

signature(Handler, Opts) ->
    case Handler:teacup@signature(Opts) of
        ok -> Handler;
        {ok, Signature} -> Signature;
        {error, Reason} -> throw({signature_error, Reason})
    end.

run_ref(Fun, TRef) ->
    case teacup_registry:lookup(TRef) of
        not_found -> {error, not_found};
        {ok, Pid} -> Fun(Pid)
    end.
