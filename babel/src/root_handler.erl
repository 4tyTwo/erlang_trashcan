-module(root_handler).

-export[init/2].
-export[create_noice/1].

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            PQs = cowboy_req:parse_qs(Req0),
            Map = maps:from_list(PQs),
            case maps:get(<<"len">>, Map, not_found) of
                LenString ->
                    {Length, <<>>} = string:to_integer(LenString);
                not_found ->
                    Length = 0
            end,
            Req = cowboy_req:stream_reply(200, Req0),
            cowboy_req:stream_body(create_noice(Length) ++ "\r\n", nofin, Req);
        _ ->
            Req = cowboy_req:stream_reply(405, Req0),
            ok
    end,
    {ok, Req, Opts}.

create_noice(Length) ->
    noice([], Length, newWord).

noice(Acc, 0, _) ->
    lists:reverse(Acc);
noice(Acc, Length, oldWord) ->
    NewVal = random_lowercase_or_ws(),
    NewAcc = [NewVal| Acc],
    case NewVal of
        32 ->
            noice(NewAcc, Length - 1, newWord);
        _ ->
            noice(NewAcc, Length - 1, oldWord)
    end;
noice(Acc, Length, newWord) ->
    NewAcc = [random_uppercase() | Acc],
    noice(NewAcc, Length - 1, oldWord).

random_uppercase() ->
    65 + rand:uniform(25).

random_lowercase_or_ws() ->
    case rand:uniform(7) > 1 of
        true ->
            97 + rand:uniform(25);
        false ->
            32
    end.
