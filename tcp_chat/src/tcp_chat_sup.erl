%%%-------------------------------------------------------------------
%% @doc tcp_chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    lager:notice("App supervisor initialized"),
    SupArgs = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    Room = #{
        id => my_room,
        start => {room2,start_link,[]}
    },
    Room_tcp = #{
        id => my_room_tcp,
        start => {room_tcp_server,start_link,[]}
    },
    Client_tcp = #{
        id => my_client_tcp,
        start => {client_tcp_server,start_link,[]}
    },
    {ok, { SupArgs, [Room, Room_tcp, Client_tcp]} }.

%%====================================================================
%% Internal functions
%%====================================================================
