-module(client_tcp_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3]).

-export([send/1]).

send(Msg) ->
    gen_server:cast(?MODULE, {send, Msg}).

start_link() ->
    gen_server:start_link({local,client_tcp_server},?MODULE,undefined,[]).

init(undefined) ->
    lager:notice("Client tcp server initializing"),
    Host = "localhost",
    Port = 1234,
    Result = gen_tcp:connect(Host,Port,[binary,{active,true},{packet, 2}],1000),
    case Result of
        {ok, Socket} ->
            {ok, #{host => Host, port => Port, socket => Socket}};
        {error, timeout} ->
            lager:warning("Server connection timeout"),
            exit(timeout)
    end.

handle_cast({send, Msg}, State) ->
    Socket = maps:get(socket, State),
    lager:info("Sending a message to room_tcp_server"),
    gen_tcp:send(Socket,binary_encoding:encode(Msg)),
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.
