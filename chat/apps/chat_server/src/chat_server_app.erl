-module(chat_server_app).

-behaviour(application).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2]).
-export([stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(Type :: term(), Args :: term()) ->
    {ok , pid()}.

start(_Type, _Args) ->
    ok = lager:notice("Application start"),
    {ok, _} = chat_server_sup:start_link().

-spec stop(_State :: term()) ->
    ok.

stop(_State) ->
    ok.
