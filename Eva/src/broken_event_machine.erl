-module(broken_event_machine).
-behaviour(gen_event).

-export([init/1]).
-export([handle_info/2]).
-export([handle_call/2]).
-export([handle_event/2]).


init([]) ->
    {ok, []}.

handle_event({summ, Arg1, Arg2}, State) ->
    calculator:summ(Arg2, Arg1),
    {ok, State};

handle_event({substract, Arg1, Arg2}, State) ->
    calculator:substract(Arg2, Arg1),
    {ok, State};

handle_event({multiply, Arg1, Arg2}, State) ->
    calculator:multiply(Arg2, Arg1),
    {ok, State};

handle_event({divide, Arg1, Arg2}, State) ->
    calculator:divide(Arg2, Arg1),
    {ok, State};

handle_event({power, Arg, Power}, State) ->
    calculator:power(Power, Arg),
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, State, State}.
