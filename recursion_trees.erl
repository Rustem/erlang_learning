-module(recursion_trees).
-export([empty/0, insert/3, lookup/2, has_value/2, bhas_value/2]).

%% each node contains {key, val, {Smaller, Larger}}
%% where Smaller and Larger are again nodes that contain
%% same info


empty() -> {node, 'nil'}.

insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};

insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};

insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};

insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.


lookup(_, {node, 'nil'}) ->
    undefined;

lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};

lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
    lookup(Key, Smaller);

lookup(Key, {node, {_, _, _, Larger}}) ->
    lookup(Key, Larger).

%% given value VAl, check does tree contains it
has_value(_, {node, 'nil'}) -> false;
has_value(Val, {node, {_, Val, _, _}}) -> true;
has_value(Val, {node, {_, _, Left, Right}}) ->
    case has_value(Val, Left) of
        true -> true;
        false -> has_value(Val, Right)
    end.

%% better has value

bhas_value(Val, T) ->
    try has_value1(Val, T) of
        false -> false
    catch
        true -> true
    end.

has_value1(_, {node, 'nil'}) -> false;
has_value1(Val, {node, {_, Val, _, _}}) -> throw(true);
has_value1(Val, {node, {_, _, Left, Right}}) ->
     has_value1(Val, Left),
     has_value1(Val, Right).

