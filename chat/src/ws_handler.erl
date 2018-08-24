-module(ws_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: atom().

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec register(chat_server:username()) ->
    ok.
register(Username) ->
    lager:info("Registrating user ~p", [Username]),
    chat_server:register_connection(Username, self()),
    ok.

-spec send(chat_server:message()) ->
    ok.
send(Message) ->
    lager:info("Websocket handler caught message: ~p", [Message]),
    chat_server:send(Message, self()),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({tcp, http}, term(), list()) ->
    {upgrade, protocol, cowboy_websocket}.

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:notice("Initializing websocket, PID: ~p", [self()]),
    {ok, Req, connected}.

websocket_handle({text, Message}, Req, State) ->
    Test = protocol:json_to_server_message(Message, self()),
    lager:info("Decoded JSON: ~p", [Test]),
    chat_server:send(Test),
    {ok, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({send, Message}, Req, State) ->
    Json = protocol:message_to_json(Message), % А не ебанет?
    {reply, {text, Json}, Req, State}.

-spec websocket_terminate(term(), term(), state()) ->
    ok.
websocket_terminate(_Reason, _Req, _State) ->
    lager:info("Websocket process ~p is terminated", [self()]),
    ok.
