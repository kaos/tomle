%%% @author Andreas Stenius <andreas.stenius@astekk.se>
%%% @copyright (C) 2013, Andreas Stenius
%%% @doc
%%%
%%% @end
%%% Created :  7 Mar 2013 by Andreas Stenius <andreas.stenius@astekk.se>

-module(tomle_main).
-export([main/1]).

%% Currently, it reads TOML data on stdin, and spits it out as JSON on stdout,
%% according to the format requirements of toml-test.

%% Future work should add options to control what it is to do.

main(_Args) ->
    case tomle:file(standard_io) of
        {ok, R} ->
            %%io:format("Parsed: ~p~n~n", [R]),
            io:format("{~s}", [lists:flatten(tomle2json(R, []))]);
        {error, {Line, Mod, Msg}} ->
            io:format(standard_error, "~s:~b: ~s~n", ["stdin", Line, Mod:format_error(Msg)]),
            halt(1)
    end.

tomle2json([], Acc) -> string:join(lists:reverse(Acc), ",\n");
tomle2json([{Group, Values}|T], Acc) when is_tuple(hd(Values)) ->
    tomle2json(
      T, 
      [[$",Group,$",$:,${,$\n, 
        tomle2json(Values, []), $}]
       |Acc]
     );
tomle2json([{Key, Value}|T], Acc) ->
    tomle2json(T, [hash2json(Key, Value)|Acc]).

hash2json(K, V) ->
    [$", K, $", $:|value2json(V)]. 
    
value2json(V) when is_list(V) ->
    case type(V) of
        string ->
            json("string", V);
        array ->
            [json("array"),$[,
             string:join([value2json(A) || A <- V], ","),
             $]]
    end;
value2json({{Y,Mo,D},{H,Mi,S}}) ->
    json("datetime",
         io_lib:format(
           "~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0bZ",
           [Y, Mo, D, H, Mi, S]));
value2json(V) when is_integer(V) ->
    json("integer", integer_to_list(V));
value2json(B) when B==true; B==false ->
    json("boolean", atom_to_list(B)).
    
type([]) ->
    string;
type([C|S]) when
      is_integer(C)
      andalso C =< 255
      andalso (C >= 32 orelse C == 9 orelse 
               C == 10 orelse C == 12 orelse 
               C == 13) ->
    type(S);
type(_) ->
    array.

json(T) ->
    ["{\"type\":\"",T,"\",\"value:\":"].

json(T, V) ->
    [json(T),$",V,$",$}].
