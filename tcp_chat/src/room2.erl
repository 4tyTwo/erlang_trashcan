-module(room2).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3]).

-export([send/2,send/1]).

send(Username, Message) ->
    gen_server:cast(?MODULE, {send, {Username, Message}}).
send(Message) ->
    send("Incognito",Message).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, room2}, ?MODULE, undefined,[]).

init(undefined) ->
    lager:notice("Initialized room2"),
    {ok, #{users => [], pids => []}}.

handle_cast({send, {Username, Message}}, State) ->
    lager:info("Got a message in room"),
    io:format("~p: ~p~n",[Username, Message]),
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.
