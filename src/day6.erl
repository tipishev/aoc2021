-module(day6).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    parse_int_array(File).

part2(File) ->
    parse_int_array(File).

%%% Parser

parse_int_array(Filename) ->
    {ok, CsvStrIntegers} = file:read_file(Filename),
    StrIntegers = string:lexemes(CsvStrIntegers, ",\n"),
    _Integers = [binary_to_integer(StrInteger) || StrInteger <- StrIntegers].
