% Copyright (c) 2016, Yuce Tekol <yucetekol@gmail.com>.
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:
%
% * Redistributions of source code must retain the above copyright
%   notice, this list of conditions and the following disclaimer.
%
% * Redistributions in binary form must reproduce the above copyright
%   notice, this list of conditions and the following disclaimer in the
%   documentation and/or other materials provided with the distribution.
%
% * The names of its contributors may not be used to endorse or promote
%   products derived from this software without specific prior written
%   permission.
%
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

-opaque teacup_ref() :: {teacup@ref, Handler :: atom(), reference()}.
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

-spec pid@(TRef :: teacup_ref()) -> {error, not_found} | {ok, pid()}.
pid@(TRef) ->
    teacup_registry:pid(TRef).

%% == Internal

signature(Handler, Opts) ->
    case Handler:teacup@signature(Opts) of
        ok -> Handler;
        {ok, Signature} -> Signature;
        {error, Reason} -> throw({signature_error, Reason})
    end.

run_ref(Fun, TRef) ->
    case teacup_registry:pid(TRef) of
        not_found ->
            {error, not_found};
        {ok, Pid} ->
            Fun(Pid)
    end.
