-module(day7).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Positions = parse_int_array(File),
    brute_force_target(Positions, fun linear_fuel_cost/2).

part2(File) ->
    Positions = parse_int_array(File),
    brute_force_target(Positions, fun progressive_fuel_cost/2).

linear_fuel_cost(Positions, Target) when is_list(Positions) ->
    FuelCosts = [linear_fuel_cost(Position, Target) || Position <- Positions],
    lists:sum(FuelCosts);
linear_fuel_cost(Position, Target) when is_integer(Position) ->
    abs(Position - Target).

progressive_fuel_cost(Positions, Target) when is_list(Positions) ->
    FuelCosts = [progressive_fuel_cost(Position, Target) || Position <- Positions],
    lists:sum(FuelCosts);
progressive_fuel_cost(Position, Target) when is_integer(Position) ->
    Distance = abs(Position - Target),
    % a simplified sum of arithmetic progression
    round(Distance * (Distance + 1) / 2).

brute_force_target(Positions, FuelCostFun) ->
    {Min, Max} = {lists:min(Positions), lists:max(Positions)},
    Targets = lists:seq(Min, Max),
    FuelCosts = [FuelCostFun(Positions, Target) || Target <- Targets],
    lists:min(FuelCosts).

parse_int_array(Filename) ->
    {ok, CsvStrIntegers} = file:read_file(Filename),
    StrIntegers = string:lexemes(CsvStrIntegers, ",\n"),
    _Integers = [binary_to_integer(StrInteger) || StrInteger <- StrIntegers].
