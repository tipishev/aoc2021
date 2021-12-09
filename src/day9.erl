-module(day9).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Rows = read_integer_lists(File),
    Index = index(Rows),
    {MaxX, MaxY} = {length(Rows), length(hd(Rows))},
    Xs = lists:seq(1, MaxX),
    Ys = lists:seq(1, MaxY),
    Heights = [maps:get({X, Y}, Index) || X <- Xs, Y <- Ys, is_min(X, Y, Index)],
    RiskLevels = [H + 1 || H <- Heights],
    lists:sum(RiskLevels).

is_min(X, Y, Index) ->
    % io:format("X: ~p, Y: ~p, Index: ~p", [X, Y, Index]),
    Value = maps:get({X, Y}, Index),
    Neighbors = [
        maps:get({Nx, Ny}, Index)
        || {Nx, Ny} <- neighbors({X, Y}),
           % a sloppy check for borders
           maps:is_key({Nx, Ny}, Index)
    ],
    BiggerNeighbors = [N || N <- Neighbors, N > Value],
    BiggerNeighbors =:= Neighbors.



part2(File) ->
    read_integer_lists(File).

% Neighbors
% NOTE: does not check the board borders
neighbors({X, Y}) ->
    [{NeighbX, NeighbY} || {NeighbX, NeighbY} <- [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}]].

% Indexer

index(Rows) ->
    EnumeratedRows = enumerate([enumerate(Row) || Row <- Rows]),
    index(EnumeratedRows, _Index = #{}).

index([], Index) -> Index;
index([{RowIndex, Row} | Rows], Index) ->
    index(Rows, index_row(RowIndex, Row, Index)).

index_row(_RowIndex, [], Index) -> Index;
index_row(RowIndex, [{ColIndex, Value} | Cols], Index) ->
    index_row(RowIndex, Cols, Index#{{RowIndex, ColIndex} => Value}).

% Parser

read_integer_lists(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [to_integers_list(Line) || Line <- Lines].

to_integers_list(BinInteger) -> [Char - 48 || <<Char>> <= BinInteger].

% Helpers

% Python, I miss you sometimes :'-[
enumerate(List) -> lists:zip(lists:seq(1, length(List)), List).
