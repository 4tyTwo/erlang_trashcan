-module(binary_encoding).

-export([encode/1,decode/1]).

%Функции для парса, если такие будут, тоже сюда

encode(Msg) when is_binary(Msg) ->
    Msg;
encode(Msg) ->
    term_to_binary(Msg).

decode(Msg) when is_binary(Msg) ->
    binary_to_term(Msg);
decode(Msg) ->
    Msg.
