-module(room_tcp_server).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3]).

-export([room/1, accept/2, loop/3]).

start(Port) ->
    spawn(?MODULE,room,[Port]).

room(Port) ->
    lager:notice("Room started on port ~p",[Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, 2}]),
    [spawn(?MODULE,accept,[Id,ListenSocket]) || Id <- lists:seq(1,3)],
    receive
        stop_room ->
            lager:notice("Room is stopped by command"),
            exit(stopped)
    end.

accept(Id,ListenSocket) ->
    lager:info("Socket ~p is waiting",[Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    lager:info("Socket ~p is now busy",[Id]),
    loop(Id,Socket,ListenSocket).


loop(Id, Socket, ListenSocket) ->
    case gen_tcp:recv(Socket,0) of
        {ok, Msg} ->
            lager:info("Got a message in room_tcp_server"),
            room2:send(binary_encoding:decode(Msg)),
            loop(Id,Socket,ListenSocket);
        {error, closed} ->
            lager:info("Socket ~p stopped",[Socket]),
            accept(Id,ListenSocket)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local,room_tcp_server},?MODULE,undefined,[]).

init(undefined) ->
    start(1234),
    {ok,[]}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.
