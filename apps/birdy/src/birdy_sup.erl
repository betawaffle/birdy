-module(birdy_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor Callbacks
-export([init/1]).


%% =============================================================================
%% API Functions
%% =============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% =============================================================================
%% Supervisor Callbacks
%% =============================================================================

init(_) ->
    Starter = {riak_core_vnode_master, start_link, [birdy_vnode]},
    VMaster = {birdy_vnode_master, Starter,
               permanent, 5000,
               worker, [riak_core_vnode_master]},
    Restart = {one_for_one, 5, 10},
    SupSpec = {Restart, [VMaster]},
    {ok, SupSpec}.
