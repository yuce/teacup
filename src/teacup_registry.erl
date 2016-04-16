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

-module(teacup_registry).

-export([update/2,
         remove/1,
         lookup/1]).

%% == API

update(Key, Value) ->
    true = ets:insert(teacup_registry, {Key, Value}),
    ok.

remove(Key) ->
    true = ets:delete(teacup_registry, Key),
    ok.

lookup(Key) ->
    case ets:lookup(teacup_registry, Key) of
        [] -> not_found;
        [{Key, Pid}] -> {ok, Pid}
    end.
