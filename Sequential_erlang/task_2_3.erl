-module(task_2_3).

-export([test/0]).
-export([fold/3]).

fold(Fun,Acc,Map) ->
  fold_1(Fun,Acc,maps:to_list(Map)).

fold_1(_,Acc,[]) ->
  Acc;
fold_1(Fun,Acc,[{Key, Value} | Tail]) ->
  fold_1(Fun,Fun(Key,Value,Acc),Tail).

test() ->
  Fun = fun(_, V, AccIn) -> AccIn + V end,
  Map = #{key => 1, key2 => 6, key7 => 4},
  Acc = 0,
  Sample = maps:fold(Fun, Acc, Map),
  Sample = fold(Fun,0,Map),
  io:format("Test passed~n",[]).
