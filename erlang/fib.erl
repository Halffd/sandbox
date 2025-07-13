-module(fib).
-export([fibonacci/1]).

fibonacci(N) when N < 2 ->
    io:format("Computing fibonacci(~p)~n", [N]),
    N;
fibonacci(N) ->
    io:format("Computing fibonacci(~p)~n", [N]),
    fibonacci(N - 1) + fibonacci(N - 2).
