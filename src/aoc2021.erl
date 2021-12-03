-module(aoc2021).

%% API exports
-export([main/1, solve/3]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc a wrapper around solve/3, can be run as escript
main([DayStr, PartStr]) ->
    main([DayStr, PartStr, "real"]);
main([DayStr, PartStr, InputTypeStr]) ->
    io:format("Solving ~s ~s with ~s input.~n", [DayStr, PartStr, InputTypeStr]),
    Day = list_to_atom(DayStr),
    Part = list_to_atom(PartStr),
    InputType = list_to_atom(InputTypeStr),
    Result = solve(Day, Part, InputType),
    io:format("Result: ~p~n", [Result]),
    erlang:halt(0).

%% @doc an interactive shell function
solve(Day, Part, InputType) ->
    DayStr = atom_to_list(Day),
    InputFilename =
        case InputType of
            example -> DayStr ++ "_example.txt";
            real -> DayStr ++ ".txt"
        end,
    InputFilePath = filename:join("inputs", InputFilename),
    Day:Part([InputFilePath]).
