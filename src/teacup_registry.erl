-module(teacup_registry).

-export([start_link/0,
         update/2,
         remove/1,
         pid/1]).

start_link() ->
    simpre:start_link({local, ?MODULE}).

update(Ref, Pid) ->
    simpre:update({teacup_registry, Ref}, Pid).

remove(Ref) ->
    simpre:remove({teacup_registry, Ref}).

pid(Ref) ->
    simpre:pid({teacup_registry, Ref}).
