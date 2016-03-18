-module(proxy@tc).
-behaviour(teacup_server).

-export([teacup@init/1,
         teacup@status/2,
         teacup@data/2,
         teacup@error/2]).

-define(MSG, ?MODULE).

teacup@init(Opts) ->
    {ok, Opts}.

teacup@status(Status, State) ->    
    notify_parent({teacup@status, Status}, State),
    {ok, State}.
    
teacup@data(Data, State) ->
    notify_parent({teacup@data, Data}, State),
    {ok, State}.
    
teacup@error(Reason, State) ->
    notify_parent({teacup@error, Reason}, State),
    {error, Reason}.    
    
notify_parent(Message, #{parent@ := Parent,
                         ref@ := Ref}) ->
    Parent ! {?MSG, teacup:ref(Ref), Message}.