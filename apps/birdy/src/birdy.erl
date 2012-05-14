-module(birdy).

%% API
-export([ping/0]).
-export([add_user/1,
         add_users/1,
         remove_user/1,
         remove_users/1]).
-export([dev_join/0]).


%% Includes
-include("birdy.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").


%% Public API

add_user(Id) ->
    birdy_vnode:add_user(Id).

add_users(Ids) ->
    lists:foreach(fun add_user/1, Ids).

remove_user(Id) ->
    birdy_vnode:remove_user(Id).

remove_users(Ids) ->
    lists:foreach(fun remove_user/1, Ids).

% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, birdy),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, birdy_vnode_master).


dev_join() ->
    Self = node(),
    lists:foreach(fun (Node) when Node =/= Self ->
                          riak_core:join(Node);
                      (_) ->
                          ok
                  end, ['birdy1@127.0.0.1',
                        'birdy2@127.0.0.1',
                        'birdy3@127.0.0.1']).


