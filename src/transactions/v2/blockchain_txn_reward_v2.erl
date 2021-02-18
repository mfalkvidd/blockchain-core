%%%-------------------------------------------------------------------
%% @doc This module implements reward_v2 items which only record an
%% account and an amount.
%%%-------------------------------------------------------------------
-module(blockchain_txn_reward_v2).

-behavior(blockchain_json).
-include("blockchain_json.hrl").

-include("blockchain_utils.hrl").
-include_lib("helium_proto/include/blockchain_txn_rewards_v2_pb.hrl").

-export([
    new/2,
    hash/1,
    account/1,
    amount/1,
    is_valid/1,
    print/1,
    to_json/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type reward_v2() :: #blockchain_txn_reward_v2_pb{}.
-type rewards_v2() :: [reward_v2()].

-export_type([reward_v2/0, rewards_v2/0]).

%%--------------------------------------------------------------------
%% @doc
%% Create a new reward_v2 item
%% @end
%%--------------------------------------------------------------------
-spec new(Account :: libp2p_crypto:pubkey_bin(),
          Amount :: non_neg_integer()) -> reward_v2().
new(Account, Amount) when is_binary(Account)
                          andalso is_integer(Amount)
                          andalso Amount > 0 ->
    #blockchain_txn_reward_v2_pb{
        account=Account,
        amount=Amount
    }.

%%--------------------------------------------------------------------
%% @doc
%% Calculate the hash of a reward v2 item.
%% @end
%%--------------------------------------------------------------------
-spec hash(reward_v2()) -> blockchain_txn:hash().
hash(Reward) ->
    EncodedReward = blockchain_txn_rewards_v2_pb:encode_msg(Reward),
    crypto:hash(sha256, EncodedReward).

%%--------------------------------------------------------------------
%% @doc
%% Return the account associated with a reward v2 item.
%% @end
%%--------------------------------------------------------------------
-spec account(Reward :: reward_v2()) -> libp2p_crypto:pubkey_bin().
account(Reward) ->
    Reward#blockchain_txn_reward_v2_pb.account.

%%--------------------------------------------------------------------
%% @doc
%% Return the amount associated with a reward v2 item.
%% @end
%%--------------------------------------------------------------------
-spec amount(Reward :: reward_v2()) -> non_neg_integer().
amount(Reward) ->
    Reward#blockchain_txn_reward_v2_pb.amount.

%%--------------------------------------------------------------------
%% @doc
%% Determine if a reward_v2 item is valid.
%% @end
%%--------------------------------------------------------------------
-spec is_valid(Reward :: reward_v2()) -> ok | {error, Reason :: term()}.
is_valid(#blockchain_txn_reward_v2_pb{account=Account, amount=Amount}) ->
    case erlang:is_binary(Account) andalso Amount > 0 of
        true -> ok;
        false -> {error, invalid_reward_v2}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Return iodata of the components of a reward_v2 item.
%% @end
%%--------------------------------------------------------------------
-spec print(reward_v2()) -> iodata().
print(undefined) -> <<"type=reward undefined">>;
print(#blockchain_txn_reward_v2_pb{account=Account, amount=Amount}) ->
    io_lib:format("type=reward_v2 account=~p, amount=~p",
                  [?TO_B58(Account), Amount]).

%%--------------------------------------------------------------------
%% @doc
%% Export reward_v2 data in a form compatible with the JSON emitter.
%% @end
%%--------------------------------------------------------------------
-spec to_json(reward_v2(), blockchain_json:opts()) -> blockchain_json:json_object().
to_json(Reward, _Opts) ->
    #{
      account => ?BIN_TO_B58(account(Reward)),
      amount => amount(Reward)
     }.


%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

new_test() ->
    Reward = #blockchain_txn_reward_v1_pb{
        account= <<"account">>,
        gateway= <<"gateway">>,
        amount= 12,
        type= poc_challengees
    },
    ?assertEqual(Reward, new(<<"account">>, <<"gateway">>, 12, poc_challengees)).

account_test() ->
    Reward = new(<<"account">>, <<"gateway">>, 12, poc_challengees),
    ?assertEqual(<<"account">>, account(Reward)).

gateway_test() ->
    Reward = new(<<"account">>, <<"gateway">>, 12, poc_challengees),
    ?assertEqual(<<"gateway">>, gateway(Reward)).

amount_test() ->
    Reward = new(<<"account">>, <<"gateway">>, 12, poc_challengees),
    ?assertEqual(12, amount(Reward)).

type_test() ->
    Reward = new(<<"account">>, <<"gateway">>, 12, poc_challengees),
    ?assertEqual(poc_challengees, type(Reward)).

to_json_test() ->
    Reward = new(<<"account">>, <<"gateway">>, 12, poc_challengees),
    Json = to_json(Reward, []),
    ?assert(lists:all(fun(K) -> maps:is_key(K, Json) end,
                      [account, gateway, amount, type])).

-endif.
