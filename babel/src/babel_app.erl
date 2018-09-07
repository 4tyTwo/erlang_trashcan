%%%-------------------------------------------------------------------
%% @doc babel public API
%% @end
%%%-------------------------------------------------------------------

-module(babel_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_',
        [
            {"/", root_handler, []}
        ]}
    ]),
    NumAcceptors = 10,
    TransOpts = [{ip, {0, 0, 0, 0}}, {port, 7000}],
    ProtoOpts = #{env => #{dispatch => Dispatch}},
    {ok, _} = cowboy:start_clear(http, TransOpts, ProtoOpts),
    babel_sup:start_link().
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
