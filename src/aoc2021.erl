-module(aoc2021).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Day, PartStr, InputType]) ->
    io:format("Solving ~s ~s with ~s input.~n", [Day, PartStr, InputType]),
    Mod = list_to_atom(Day),
    Part = list_to_atom(PartStr),
    InputFile =
        case InputType of
            "example" -> Day ++ "_example.txt";
            "real" -> Day ++ ".txt"
        end,
    PathToInput = filename:join("inputs", InputFile),
    Result = apply(Mod, solve, [Part, PathToInput]),
    io:format("Result: ~p~n", [Result]),
    erlang:halt(0).
