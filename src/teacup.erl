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

-module(teacup).

-define(REF(Ref), {teacup@ref, Ref}).

%% == API

new(Handler, Opts) ->
    Parent = self(),
    Ref = make_ref(),
    case teacup_server:start_link(Parent, Ref, Handler, Opts) of
        {ok, Pid} ->
            {ok, Pid},
            teacup_registry:update(Ref, Pid),
            ?REF(Ref);
        Other ->
            Other
    end.

connect(?REF(Ref)) ->
    run_ref(fun(P) -> teacup_server:connect(P) end, Ref).

disconnect(?REF(Ref)) ->
    run_ref(fun(P) -> teacup_server:disconnect(P) end, Ref).

send(?REF(Ref), What) ->
    run_ref(fun(P) -> teacup_server:send(P, What) end, Ref).

%% == Internal

run_ref(Fun, Ref) ->
    case teacup_registry:pid(Ref) of
        not_found ->
            {error, not_found};
        {ok, Pid} ->
            Fun(Pid)
    end.
