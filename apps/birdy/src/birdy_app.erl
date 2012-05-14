-module(birdy_app).
-behaviour(application).

%% Application Callbacks
-export([start/2, stop/1]).

-define(SERVICE_NAME, birdy).
-define(VNODE_MODULE, birdy_vnode).
-define(RING_HANDLER, birdy_ring_event_handler).
-define(NODE_HANDLER, birdy_node_event_handler).


%% =============================================================================
%% Application Callbacks
%% =============================================================================

start(_StartType, _StartArgs) ->
    case birdy_sup:start_link() of
        {ok, Pid} ->
            started(Pid),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.


%% =============================================================================
%% Helper Functions
%% =============================================================================

started(_) ->
    ok = riak_core:register([{vnode_module, ?VNODE_MODULE}]),
    %%ok = riak_core_ring_events:add_guarded_handler(?RING_HANDLER, []),
    %%ok = riak_core_node_watcher_events:add_guarded_handler(?NODE_HANDLER, []),
    ok = riak_core_node_watcher:service_up(?SERVICE_NAME, self()).
