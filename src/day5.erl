-module(day5).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Vents = parse_vectors(File),
    Vents.

part2(File) ->
    parse_vectors(File).


%%% Parser

parse_vectors(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    StrVectors = string:lexemes(FileContent, "\n"),
    [parse_vector(StrVector) || StrVector <- StrVectors].

parse_vector(StrVector) ->
    [StrPointA, <<"->">>, StrPointB] = string:lexemes(StrVector, " "),
    PointA = parse_point(StrPointA),
    PointB = parse_point(StrPointB),
    {PointA, PointB}.

parse_point(StrPoint) ->
    [StrX, StrY] = string:lexemes(StrPoint, ","),
    X = binary_to_integer(StrX),
    Y = binary_to_integer(StrY),
    {X, Y}.

