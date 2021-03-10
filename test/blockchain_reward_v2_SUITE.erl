-module(blockchain_reward_v2_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("blockchain_vars.hrl").

-export([
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         reward_v2_test/1
        ]).

suite() ->
    [{timetrap,{seconds,200}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(lager),

    {ok, Dir} = file:get_cwd(),
    PrivDir = filename:join([Dir, "priv"]),
    NewDir = PrivDir ++ "/ledger/",
    ok = filelib:ensure_dir(NewDir),

    os:cmd("wget https://blockchain-core.s3-us-west-1.amazonaws.com/snap-591841"),

    Filename = Dir ++ "/snap-591841",

    {ok, BinSnap} = file:read_file(Filename),

    {ok, Snapshot} = blockchain_ledger_snapshot_v1:deserialize(BinSnap),
    SHA = blockchain_ledger_snapshot_v1:hash(Snapshot),

    {ok, _GWCache} = blockchain_gateway_cache:start_link(),
    {ok, _Pid} = blockchain_score_cache:start_link(),

    {ok, BinGen} = file:read_file("../../../../test/genesis"),
    GenesisBlock = blockchain_block:deserialize(BinGen),
    {ok, Chain} = blockchain:new(NewDir, GenesisBlock, blessed_snapshot, undefined),

    {ok, Ledger1} = blockchain_ledger_snapshot_v1:import(Chain, SHA, Snapshot),
    {ok, Height} = blockchain_ledger_v1:current_height(Ledger1),

    ct:pal("loaded ledger at height ~p", [Height]),

    [{chain, Chain} | Config].

end_per_suite(_Config) ->
    blockchain_score_cache:stop(),
    ok.

all() ->
    [reward_v2_test].

reward_v2_test(Config) ->
    Chain = ?config(chain, Config),
    Ledger = blockchain:ledger(Chain),

    {ok, Height} = blockchain_ledger_v1:current_height(Ledger),

    {Time, RewardsV1} =
        timer:tc(
          fun() ->
                  {ok, _} = blockchain_txn_rewards_v1:calculate_rewards(Height - 15, Height, Chain)
          end),
    ct:pal("rewards v1: ~p ms", [Time div 1000]),

    {Time2, RewardsV2} =
        timer:tc(
          fun() ->
                  blockchain_txn_rewards_v2:calculate_rewards(Height - 15, Height, Chain)
          end),
    ct:pal("rewards v2: ~p ms", [Time2 div 1000]),

    true = blockchain_txn_rewards_v2:v1_to_v2(RewardsV1) == RewardsV2.
