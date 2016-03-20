-module('rebar3_protobuffs').

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = 'rebar3_protobuffs_prv':init(State),
    {ok, State2} = 'rebar3_protobuffs_prv_clean':init(State1),
    {ok, State2}.
