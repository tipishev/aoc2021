-module(day9).

%% API exports
-export([part1/1, part2/1]).
part1(File) ->
    Grid = read_grid(File),
    {MaxX, MaxY} = dimensions(Grid),
    {Xs, Ys} = {one_to(MaxX), one_to(MaxY)},
    ValueIndex = index_values(Grid),
    Heights = [
        maps:get({X, Y}, ValueIndex)
        || X <- Xs, Y <- Ys, is_local_minimum(X, Y, ValueIndex)
    ],
    RiskLevels = [Height + 1 || Height <- Heights],
    sum(RiskLevels).

dimensions(Rows) -> {length(Rows), length(hd(Rows))}.

% NOTE: does not check the grid borders
adjacent({X, Y}) ->
    [
        {AdjacentX, AdjacentY}
        || {AdjacentX, AdjacentY} <- [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}]
    ].

is_local_minimum(X, Y, Index) ->
    Value = maps:get({X, Y}, Index),
    Adjacents = [
        maps:get({AdjacentX, AdjacentY}, Index)
        || {AdjacentX, AdjacentY} <- adjacent({X, Y}),
           % a sloppy check for borders
           maps:is_key({AdjacentX, AdjacentY}, Index)
    ],
    LargerAdjacents = [Adjacent || Adjacent <- Adjacents, Adjacent > Value],
    LargerAdjacents =:= Adjacents.

part2(File) ->
    _Rows = read_grid(File).
    % ValueIndex = index(Rows),
    % {MaxX, MaxY} = {length(Rows), length(hd(Rows))},
    % io:format("~p, ~p~n", [MaxX, MaxY]),
    % neighbors({1, 10}, {MaxX, MaxY}).
    % Xs = lists:seq(1, MaxX),
    % Ys = lists:seq(1, MaxY),
    % Walls = [{X, Y} || X <- Xs, Y <- Ys, maps:get({X, Y}, Index) =:= 9],
    % length(Walls).

% adjacent({X, Y}, {MaxX, MaxY}) ->
%     [
%         {NeighbX, NeighbY}
%         || {NeighbX, NeighbY} <- [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}],
%            NeighbX > 0,
%            NeighbY > 0,
%            NeighbX =< MaxX,
%            NeighbY =< MaxY
%     ].



%% @doc Produces a {X, Y} -> Value map from a list of row values.
index_values(Rows) ->
    EnumeratedRows = enumerate([enumerate(Row) || Row <- Rows]),
    index_values(EnumeratedRows, _Index = #{}).

index_values([], Index) -> Index;
index_values([{RowIndex, Row} | Rows], Index) ->
    index_values(Rows, index_values_row(RowIndex, Row, Index)).

index_values_row(_RowIndex, [], Index) -> Index;
index_values_row(RowIndex, [{ColIndex, Value} | Cols], Index) ->
    index_values_row(RowIndex, Cols, Index#{{RowIndex, ColIndex} => Value}).

% Parser

read_grid(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [to_grid_row(Line) || Line <- Lines].

to_grid_row(BinInteger) -> [Char - 48 || <<Char>> <= BinInteger].


% Helpers

% Python, I miss you sometimes :'-[
enumerate(List) -> lists:zip(lists:seq(1, length(List)), List).
sum(List) -> lists:sum(List).
one_to(Value) -> lists:seq(1, Value).
