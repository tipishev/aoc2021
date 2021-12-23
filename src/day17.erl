-module(day17).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    parse(Filename).

part2(Filename) ->
    parse(Filename).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [<<"target">>, <<"area">>, RangeX, RangeY] = string:lexemes(FileContent, " \n:,"),
    {parse_range(RangeX), parse_range(RangeY)}.

parse_range(<<AxisBin:1/binary, "=", Numbers/binary>>) ->
    [LowBin, HighBin] = string:lexemes(Numbers, "."),
    {binary_to_atom(AxisBin), to_int(LowBin), to_int(HighBin)}.

to_int(Bin) ->
    {Int, <<>>} = string:to_integer(Bin),
    Int.

