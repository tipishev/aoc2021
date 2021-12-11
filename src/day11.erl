-module(day11).

%% API exports
-export([part1/1, part2/1]).

-define(GRID_SIZE, 10).

part1(File) ->
    Grid = parse(File),
    iterate(Grid, 100).

part2(File) ->
    parse(File).

% Grid stuff

iterate(Grid, NumSteps) ->
    lists:foldl(fun(_Idx, GridAcc) -> step(GridAcc) end, Grid, one_to(NumSteps)).

step(Grid) -> Grid.

% erlfmt-ignore
adjacent({X, Y}) ->
    [{AdjX, AdjY}
    || AdjX <- [X - 1, X, X + 1], AdjY <- [Y - 1, Y, Y + 1],
       {AdjX, AdjY} =/= {X, Y},  % exclude self
       AdjX > 0, AdjY > 0, AdjX =< ?GRID_SIZE, AdjY =< ?GRID_SIZE].

increment(Grid) ->
    Increment = fun(_Key, Value) -> Value + 1 end,
    maps:map(Increment, Grid).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    ListOfIntegerLists = [[binary_to_integer(<<Char>>)
                    || <<Char>> <= Line] || Line <- Lines],
    index_values(ListOfIntegerLists).

% %% @doc Produces a {X, Y} -> Value map from a list of row values.
index_values(Rows) ->
    EnumeratedRows = enumerate([enumerate(Row) || Row <- Rows]),
    index_values(EnumeratedRows, _Index = #{}).
index_values([], Index) -> Index;
index_values([{RowIndex, Row} | Rows], Index) ->
    index_values(Rows, index_values_row(RowIndex, Row, Index)).
index_values_row(_RowIndex, [], Index) -> Index;
index_values_row(RowIndex, [{ColIndex, Value} | Cols], Index) ->
    index_values_row(RowIndex, Cols, Index#{{RowIndex, ColIndex} => Value}).

% Herlpers

enumerate(List) -> lists:zip(lists:seq(1, length(List)), List).
sum(List) -> lists:sum(List).
one_to(Value) -> lists:seq(1, Value).
