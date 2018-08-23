-module(protocol).

-export([encode/1, decode/1]).

 encode(Message) ->
     DataMap = put_values(Message),
     jiffy:encode(DataMap).

decode(Message) ->
    DataMap = jiffy:decode(Message, [return_maps]),
    extract_values(DataMap).

put_values({Username, Message}) ->
    #{user => Username, message => Message}.

extract_values(DataMap) ->
    Username = maps:get(user, DataMap),
    Message = maps:get(user, DataMap),
    {Username, Message}.
