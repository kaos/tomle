%%% @author Andreas Stenius <andreas.stenius@astekk.se>
%%% @copyright (C) 2013, Andreas Stenius
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2013 by Andreas Stenius <andreas.stenius@astekk.se>

-module(tomle).

-export(
   [
    file/1,
    file/2,
    string/1,
    string/2
   ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


file(F) ->
    file(F, [compile]).

file(F, Opts) when is_list(F) ->
    {ok, File} = file:open(F, [read]),
    try
        file(File, Opts)
    after
        file:close(File)
    end;
file(File, Opts) ->
    Res = parse_tokens(io:request(File, {get_until, "", tomle_scanner, tokens, []})),
    process(Res, Opts).

string(S) -> string(S, [compile]).

string(S, Opts) when is_list(S) ->
    Res = parse_tokens(tomle_scanner:string(S)),
    process(Res, Opts);
string(S, Opts) when is_binary(S) ->
    string(binary_to_list(S), Opts).


parse_tokens({ok, Tokens, _Line}) ->
    tomle_parser:parse(Tokens).

process(ParseRes, Opts) ->    
    case proplists:get_bool(compile, Opts) of
        true -> compile_parse_tree(ParseRes);
        false -> ParseRes
    end.

compile_parse_tree({ok, Tree}) ->
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




-ifdef(TEST).

check_compiled_result(example, T) ->
    ?assertEqual(
       [{"title", "TOML Example"},
        {"owner", 
         [{"name", "Tom Preston-Werner"},
          {"organization", "GitHub"},
          {"bio", "GitHub Cofounder & CEO\nLikes tater tots and beer."},
          {"dob", {{1979,05,27},{07,32,00}}}]},
        {"database",
         [{"server", "192.168.1.1"},
          {"ports", [8001, 8001, 8002]},
          {"connection_max", 5000},
          {"enabled", true}
         ]},
        {"servers",
         [{"alpha", 
           [{"ip", "10.0.0.1"}, {"dc", "eqdc10"}]},
          {"beta", 
           [{"ip", "10.0.0.2"}, {"dc", "eqdc10"}]}]},
        {"clients",
          [{"data", [["gamma", "delta"], [1, 2]]},
           {"hosts", ["alpha", "omega"]}]}
       ], T);
check_compiled_result(hard_example, T) ->
    ?assertEqual(
       [{"the",
         [{"test_string", "You'll hate me after this - #"},
          {"hard",
           [{"test_array", ["] ", " # "]},
            {"test_array2", ["Test #11 ]proved that", "Experiment #9 was a success" ]},
            {"another_test_string", " Same thing, but with a string #"},
            {"harder_test_string", " And when \"'s are in the string, along with # \""},
            {"bit#",
             [{"what?", "You don't think some user won't do that?"},
              {"multi_line_array", ["]"]}]}
           ]}]}
       ], T).

file_test() ->
    {ok, T} = file(toml_filename("example.toml")),
    %%?debugFmt("~p~n", [T]),
    check_compiled_result(example, T).

string_test() ->    
    {ok, S} = file:read_file(toml_filename("hard_example.toml")),
    {ok, T} = string(S),
    check_compiled_result(hard_example, T).

toml_filename(Filename) ->
    filename:join(["..", "priv", "toml", "tests", Filename]).
    
toml_test_file(Filename) ->
    file:open(toml_filename(Filename), [read]).

scan_file(Filename) ->
    {ok, File} = toml_test_file(Filename),
    {ok, R, _} = io:request(File, {get_until, "", tomle_scanner, tokens, []}),
    %%?debugFmt("~p~n", [R]).
    R.
    
example_toml_scanner_test() ->
    scan_file("example.toml").

hard_example_toml_scanner_test() ->
    scan_file("hard_example.toml").
    
parse_file(Filename) ->    
    case tomle_parser:parse(scan_file(Filename)) of
        {ok, _R} ->
            %%?debugFmt("~p~n", [R]),
            ok;
        {error, {Line, Mod, Msg}} ->
            ?debugFmt("ERROR: ~p:~p: ~s~n", [Filename, Line, Mod:format_error(Msg)]),
            error
    end.

example_toml_parser_test() ->
    ok = parse_file("example.toml").

hard_example_toml_parser_test() ->
    ok = parse_file("hard_example.toml").

-endif.
