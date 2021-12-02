-module(aoc2021).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

main([DayStr, PartStr]) ->
    main([DayStr, PartStr, "real"]);
main([DayStr, PartStr, InputType]) ->
    io:format("Solving ~s ~s with ~s input.~n", [DayStr, PartStr, InputType]),
    Day = list_to_atom(DayStr),
    Part = list_to_atom(PartStr),
    InputFilename =
        case InputType of
            "example" -> DayStr ++ "_example.txt";
            "real" -> DayStr ++ ".txt"
        end,
    InputFilePath = filename:join("inputs", InputFilename),
    Result = Day:Part([InputFilePath]),
    io:format("Result: ~p~n", [Result]),
    erlang:halt(0).
