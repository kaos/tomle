%%% @author Andreas Stenius <andreas.stenius@astekk.se>
%%% @copyright (C) 2013, Andreas Stenius
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2013 by Andreas Stenius <andreas.stenius@astekk.se>

-module(tomle).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.






-ifdef(TEST).

toml_test_file(Filename) ->
    file:open(filename:join(["..", "priv", "toml", "tests", Filename]), [read]).

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
        {ok, R} ->
            ?debugFmt("~p~n", [R]),
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
