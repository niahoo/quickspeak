
-module(quickspeak_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, [[Filename]]} = init:get_argument(chatfilename),
    {ok, [[Nick]]} = init:get_argument(nickname),
    Filemon = {qs_filemon, {qs_filemon, start_link, [Filename,Nick]}, permanent, 5000, worker, [qs_filemon]},
    %%CLIClient = {qs_client_cli, {qs_client_cli, start_link, [Filename,Nick]}, permanent, 5000, worker, [qs_client_cli]},
    {ok, { {one_for_one, 5, 10}, [Filemon]} }.

