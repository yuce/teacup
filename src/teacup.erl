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
