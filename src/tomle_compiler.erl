%%% @author Andreas Stenius <andreas.stenius@astekk.se>
%%% @copyright (C) 2013, Andreas Stenius
%%% @doc
%%%
%%% @end
%%% Created :  7 Mar 2013 by Andreas Stenius <andreas.stenius@astekk.se>

-module(tomle_compiler).

-export([compile/2]).

compile(Tree, _Opts) ->
    {ok, walk_tree(Tree, [])}.


get_group([], L) -> L;
get_group([{_, _, Key}|Keys], L) ->
    case lists:keyfind(Key, 1, L) of
        {Key, V} -> get_group(Keys, V);
        false -> []
    end.

set_group([], G, _) -> G;
set_group([{_, _, Key}=K|Keys], G, L) -> 
    G1 = set_group(Keys, G, get_group([K], L)),
    lists:keystore(Key, 1, L, {Key, G1}).

walk_tree([], L) -> L;
walk_tree([{group, Groupkey, Values}|Tree], L) ->
    G = walk_tree(Values, get_group(Groupkey, L)),
    walk_tree(Tree, set_group(Groupkey, G, L));
walk_tree([{{_, _, Key}, {_, _, Value}}|Tree], L) ->
    walk_tree(Tree, lists:keystore(Key, 1, L, {Key, Value}));
walk_tree([{{_, _, Key}, Array}|Tree], L) ->
    walk_tree(Tree, 
              lists:keystore(
                Key, 1, L, 
                {Key, walk_tree(lists:reverse(Array), [])}
               ));
walk_tree([{_, _, Value}|Array], Acc) ->
    walk_tree(Array, [Value|Acc]);
walk_tree([Value|Array], Acc) when is_list(Value) ->
    walk_tree(Array, [walk_tree(lists:reverse(Value), [])|Acc]).
