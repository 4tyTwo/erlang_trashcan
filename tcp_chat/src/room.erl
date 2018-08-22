-module(room).

-behaviour(gen_server).

-export([start_link/0,init/1,handle_cast/2,handle_call/3, handle_info/2, terminate/2]).

-export([connect/0,accept/2, stop/0]).

-type state() :: #{messages => iolist(), listen_socket => port()}.

connect() ->
    gen_server:call(?MODULE,connect).

-spec accept(Parent :: pid(),ListenSocket :: port()) ->
    ok.
accept(Parent,ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    gen_tcp:controlling_process(Socket,Parent),
    ok.

-spec stop() ->
    ok.
stop() ->
    gen_server:cast(?MODULE,stop).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local,room},?MODULE,undefined,[]).

-spec init(undefined) ->
    {ok, state()}.
init(undefined) ->
    %lager:notice("Initialized room"),
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, once}, {packet, 2}]),
    %lager:notice("Listening in room"),
    {ok, #{messages => [], listen_socket => ListenSocket}}.

-spec handle_cast(stop, state()) ->
    {stop,normal, state()} | {noreply, state()}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_call(connect, any(), State :: state()) ->
    {reply, ok, state()}.
handle_call(connect, _From, State) ->
    ListenSocket = maps:get(listen_socket, State),
    spawn_link(?MODULE, accept,[self(), ListenSocket]),
    {reply, ok, State}.

-spec handle_info({tcp, Socket :: port(), Message :: binary()}, State :: state()) ->
    {noreply, state()}.
handle_info({tcp, Socket, Msg}, State) ->
    Messages = maps:get(messages, State),
    {Username, Text} = binary_to_term(Msg),
    %lager:info("Room2 received a message ~p from ~p",[Text, Username]),
    FullMsg = {Username,erlang:localtime(), Text},
    gen_tcp:send(Socket,term_to_binary(FullMsg)),
    NewState = maps:put(messages,[FullMsg | Messages], State),
    {noreply, NewState}.

-spec terminate(normal,State :: state()) ->
    ok.
terminate(normal, _State) ->
    %lager:notice("Terminating room server"),
    ok.
