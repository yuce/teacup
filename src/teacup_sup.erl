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

-module(teacup_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% == API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% == Callbacks

init([]) ->
    _ = ets:new(teacup_registry, [named_table,
                                  public,
                                  {read_concurrency, true}]),
    SupSpec = {{rest_for_one, 10, 60}, [teacup_server_sup_spec()]},
    {ok, SupSpec}.

teacup_server_sup_spec() ->
    #{id => teacup_server_sup,
      start => {teacup_server_sup, start_link, []},
      restart => permanent,
      shutdown => 1000,
      type => supervisor,
      modules => [teacup_server_sup]}.
