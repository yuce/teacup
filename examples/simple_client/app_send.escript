
main([]) ->
    {ok, Conn} = teacup:new(echo, #{host => "127.0.0.1"}),
    ok = teacup:connect(Conn),
    ok = echo:say_hello(Conn),
    loop(Conn).

loop(Conn) ->
    receive
        {echo@message, Message} ->
            io:format("Received message: ~s~n", [Message]),
            ok = teacup:disconnect(Conn);
        Other ->
            io:format("Other message: ~p~n", [Other]),
            loop(Conn)
    end.