-module(day5).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Board = board_new(),
    Vents = parse_vectors(File),
    NonDiagonalVents = lists:filter(fun non_diagonal/1, Vents),
    CoveredSquares = covered_squares(NonDiagonalVents),
    CoveredBoard = board_increment(Board, CoveredSquares),
    board_count_overlaps(CoveredBoard).

part2(File) ->
    parse_vectors(File).

%%% Board helpers

board_new() -> #{}.
board_increment(Board, Squares) when is_list(Squares)->
    lists:foldl(fun(Square, BoardAcc) -> board_increment(BoardAcc, Square) end, Board, Squares);
board_increment(Board, Square) when is_tuple(Square)->
    Increment = fun(V) -> V + 1 end,
    maps:update_with(Square, Increment, _Default=1, Board).

board_count_overlaps(Board) ->
    Counts = maps:values(Board),
    length([Count || Count <- Counts, Count > 1]).

%%% Vector helpers

non_diagonal(Vector) ->
    is_horizontal(Vector) orelse is_vertical(Vector).

is_horizontal({{X, _Y1}, {X, _Y2}}) -> true;
is_horizontal({{_X1, _Y1}, {_X2, _Y2}}) -> false.

is_vertical({{_X1, Y}, {_X2, Y}}) -> true;
is_vertical({{_X1, _Y1}, {_X2, _Y2}}) -> false.

covered_squares(Vectors) when is_list(Vectors) ->
    lists:flatten([covered_squares(Vector) || Vector <- Vectors]);
covered_squares(Vector) when is_tuple(Vector) ->
    {{X1, Y1}, {X2, Y2}} = Vector,
    [SmallerX, BiggerX] = lists:sort([X1, X2]),
    [SmallerY, BiggerY] = lists:sort([Y1, Y2]),
    Xs = lists:seq(SmallerX, BiggerX),
    Ys = lists:seq(SmallerY, BiggerY),
    [{X, Y} || X <- Xs, Y <- Ys].

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

