
-define(HOST, "127.0.0.1").

main([]) ->
    Response = echo:say(?HOST, <<"Hello, World!">>),
    io:format("response: ~s~n", [Response]).

