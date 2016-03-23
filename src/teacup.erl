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
         call/2,
         cast/2]).
% -export([pid@/1]).

-define(REF(Handler, Ref), {teacup@ref, Handler, Ref}).

-opaque teacup_ref() :: {teacup@ref, Handler :: atom(), reference()}.
-export_type([teacup_ref/0]).

%% == API

new(Handler) ->
    new(Handler, #{}).

-spec new(atom(), map()) -> {ok, teacup_ref()} | {error, term()}.
new(Handler, Opts) ->
    Parent = self(),
    % TRef = ?REF(Handler, make_ref()),
    teacup_server_sup:start_child(Parent, Handler, Opts).

-spec connect(TRef :: teacup_ref()) -> ok.
connect(P) ->
    teacup_server:connect(P).

-spec connect(TRef :: teacup_ref(), Host :: binary(), Port :: integer()) ->
    ok.
connect(P, Host, Port) ->
    teacup_server:connect(P, Host, Port).

-spec disconnect(TRef :: teacup_ref()) -> ok.
disconnect(P) ->
    teacup_server:disconnect(P).

-spec send(TRef :: teacup_ref(), What :: binary()) -> ok.
send(P, What) ->
    teacup_server:send(P, What).

-spec call(TRef :: teacup_ref(), Msg :: term()) -> term().
call(P, Msg) ->
    teacup_server:call(P, Msg).

-spec cast(TRef :: teacup_ref(), Msg :: term()) -> ok.    
cast(P, Msg) ->
    teacup_server:cast(P, Msg).
