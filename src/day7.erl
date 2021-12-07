-module(day7).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Positions = parse_int_array(File),
    brute_force_target(Positions).

brute_force_target(Positions) ->
    Min = lists:min(Positions),
    Max = lists:max(Positions),
    Targets = lists:seq(Min, Max),
    FuelCosts = [fuel_cost(Positions, Target) || Target <- Targets],
    lists:min(FuelCosts).

fuel_cost(Positions, Target) when is_list(Positions) ->
    FuelCosts = [fuel_cost(Position, Target) || Position <- Positions],
    lists:sum(FuelCosts);
fuel_cost(Position, Target) when is_integer(Position) ->
    abs(Position - Target).

part2(File) ->
    parse_int_array(File).

parse_int_array(Filename) ->
    {ok, CsvStrIntegers} = file:read_file(Filename),
    StrIntegers = string:lexemes(CsvStrIntegers, ",\n"),
    _Integers = [binary_to_integer(StrInteger) || StrInteger <- StrIntegers].
