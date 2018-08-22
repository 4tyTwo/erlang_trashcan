-module(client2).

-export([start_link/0, start_link/1, init/1, handle_cast/2, handle_call/3]).
%При старте сервера должен происходить коннект к комнате (регистрация в ней тоже)
-export([start/1, send/1, stop/0, test/0]).

test() ->
    timer:sleep(200),
    start("Igor"),
    send("Hello").

send(Msg) ->
    gen_server:cast(?MODULE, {send, Msg}).

start(Username) when is_list(Username) ->
    start_link(Username).

stop() ->
    gen_server:call(?MODULE, stop).

start_link() ->
    gen_server:start_link({local,client2},?MODULE,"Incognito",[]).

start_link(Username) ->
    gen_server:start_link({local,client2},?MODULE,Username,[]).

init(Username) ->
    lager:info("Initializing client2 gen_server"),
    {ok,Username}.

handle_cast({send, Msg},Username) ->
    lager:info("Sending a message to client_tcp_server"),
    client_tcp_server:send({Username, Msg}),
    {noreply, Username}.

handle_call(stop, _From, State) ->
    exit(stopped),
    {reply, ok, State}.
