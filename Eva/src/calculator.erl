-module(calculator).

-export([summ/2]).
-export([substract/2]).
-export([multiply/2]).
-export([divide/2]).
-export([power/2]).


summ(Arg1, Arg2) ->
    Result = Arg1 + Arg2,
    io:format("~p + ~p = ~p~n", [Arg1, Arg2, Result]).

substract(Arg1, Arg2) ->
    Result = Arg1 - Arg2,
    io:format("~p - ~p = ~p~n", [Arg1, Arg2, Result]).

multiply(Arg1, Arg2) ->
    Result = Arg1 * Arg2,
    io:format("~p * ~p = ~p~n", [Arg1, Arg2, Result]).

divide(Arg1, Arg2) ->
    Result = Arg1 / Arg2,
    io:format("~p / ~p = ~p~n", [Arg1, Arg2, Result]).

power(Arg, Power) ->
    Result = math:pow(Arg, Power),
    io:format("~p ^ ~p = ~p~n", [Arg, Power, Result]).
