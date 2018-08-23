-module(protocol).



encode(Message) ->
    EJson = #{},
    jiffy:encode(EJson).

decode(Message) ->
    EJson = jiffy:decode(),
    sone_random_tranformation(EJson).
