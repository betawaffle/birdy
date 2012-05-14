-module(birdy_vnode).
-behaviour(riak_core_vnode).

-compile([{parse_transform, lager_transform}]).

%% Includes
-include("birdy.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

%% API
-export([start_vnode/1]).
-export([add_user/1,
         remove_user/1]).

%% VNode Callbacks
-export([init/1,
         handle_command/3,
         handle_exit/3,
         terminate/2]).

%% Handoff Callbacks
-export([delete/1,
         encode_handoff_item/2,
         handle_handoff_command/3,
         handle_handoff_data/2,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         is_empty/1]).

%% State Data
-record(state, {partition, part_num, users}).

-define(MASTER, birdy_vnode_master).

-define(PART_MAX, 1461501637330902918203684832716283019655932542976).
-define(PART_NUM(Partition), Partition * riak_core_config:ring_creation_size() div ?PART_MAX).


%% =============================================================================
%% API Functions
%% =============================================================================

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

add_user(Id) ->
    command({add_user, Id}, {user, Id}, 3).

remove_user(Id) ->
    command({remove_user, Id}, {user, Id}, 3).

%% --- Command Helpers ---

command(Command, Name, N) ->
    riak_core_vnode_master:command(?PREFLIST(Name, N), Command, ?MASTER).


%% =============================================================================
%% VNode Callbacks
%% =============================================================================

init([Partition]) ->
    PartNum = ?PART_NUM(Partition),
    lager:info("Starting ~p on ~p", [PartNum, node()]),
    {ok, #state{partition = Partition, part_num = PartNum, users = ordsets:new()}}.

terminate(_, S) ->
    lager:info("Stopping ~p on ~p", [S#state.part_num, node()]),
    ok.

handle_command({add_user, Id}, _, S) ->
    %% Add the user using the control stream.
    lager:info("Adding user ~s to ~p on ~p", [Id, S#state.part_num, node()]),
    {reply, {ok, S#state.partition}, do_add_user(Id, S)};
handle_command({remove_user, Id}, _, S) ->
    %% Remove the user using the control stream.
    lager:info("Removing user ~s from ~p on ~p", [Id, S#state.part_num, node()]),
    {reply, {ok, S#state.partition}, do_remove_user(Id, S)};
handle_command(ping, _, S) ->
    {reply, {pong, S#state.partition, S#state.users}, S};
handle_command(Command, _, S) ->
    lager:warning("Unhandled command ~p", [Command]),
    {noreply, S}.

handle_exit(_Pid, _Reason, S) ->
    {noreply, S}.


%% =============================================================================
%% Handoff Callbacks
%% =============================================================================

handle_handoff_command(?FOLD_REQ{foldfun = Fun, acc0 = Acc0}, _, S) ->
    {reply, do_fold(Fun, Acc0, S), S};
handle_handoff_command(_Command, _Sender, S) ->
    {noreply, S}.

encode_handoff_item({user, Id}, Id) ->
    term_to_binary({user, Id});
encode_handoff_item(ObjName, ObjValue) ->
    term_to_binary({ObjName, ObjValue}).

handle_handoff_data(Bin, S) ->
    try do_decode(Bin, S) of
        NewS = #state{} ->
            {reply, ok, NewS}
    catch
        error:Reason ->
            {reply, {error, Reason}, S}
    end.

delete(S = #state{users = undefined}) ->
    {ok, S};
delete(S) ->
    {ok, S#state{users = undefined}}.

handoff_starting(_TargetNode, S) ->
    %% Check to make sure we aren't going to create too much churn.
    {true, S}.

handoff_cancelled(S) ->
    %% Abort! Abort!
    {ok, S}.

handoff_finished(_TargetNode, S) ->
    %% Close the connection.
    {ok, S}.

is_empty(S) ->
    {ordsets:size(S#state.users) == 0, S}.


%% =============================================================================
%% Actions
%% =============================================================================

do_add_user(Id, S) ->
    S#state{users = ordsets:add_element(Id, S#state.users)}.

do_remove_user(Id, S) ->
    S#state{users = ordsets:del_element(Id, S#state.users)}.

do_decode(Bin, S) ->
    case binary_to_term(Bin) of
        {user, Id} ->
            do_add_user(Id, S);
        Other ->
            error({bad_term, Other})
    end.

-type trifold_fun(Acc) :: fun((any(), any(), Acc) -> Acc).
-type  bifold_fun(Acc) :: fun((any(), Acc) -> Acc).

-spec do_fold(trifold_fun(Acc), Acc, ordsets:ordered_set()) -> Acc.
do_fold(Fun, Acc0, S) ->
    F = fun (Id, Acc) -> Fun({user, Id}, Id, Acc)
        end,
    ordsets:fold(F, Acc0, S#state.users).
