-module(day6).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Timers = parse_int_array(File),
    length(advance(Timers, 80)).

advance(Timers, 0) ->
    Timers;
advance(Timers, Days) ->
    advance(advance(Timers), Days - 1).

advance(Timers) when is_list(Timers) ->
   lists:flatten([advance(Timer) || Timer <- Timers]);
advance(Timer) when Timer > 0 ->
    Timer - 1;
advance(Timer) when Timer =:= 0 ->
    [6, 8].


part2(File) ->
    parse_int_array(File).

%%% Parser

parse_int_array(Filename) ->
    {ok, CsvStrIntegers} = file:read_file(Filename),
    StrIntegers = string:lexemes(CsvStrIntegers, ",\n"),
    _Integers = [binary_to_integer(StrInteger) || StrInteger <- StrIntegers].
