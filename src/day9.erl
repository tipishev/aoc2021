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
    Grid = read_grid(File),
    Dimensions = dimensions(Grid),
    WallMap = to_wall_map(Grid),
    IsWallIndex = index_values(WallMap),
    IsWall = fun({X, Y}) -> maps:get({X, Y}, IsWallIndex) end,

    AdjacencyMap = adjacency_map(Dimensions, IsWall),
    AllCells = maps:keys(AdjacencyMap),

    % each cell considers itself the smallest known linked cell
    MinLink0 = maps:from_list([{Cell, Cell} || Cell <- AllCells]),
    MinLink = gossip_until_convergence(AllCells, AdjacencyMap, MinLink0),

    BasinSizes = maps:values(count(maps:values(MinLink))),
    [Biggest, SecondBiggest, ThirdBiggest | _SmallerSizes] = lists:reverse(lists:sort(BasinSizes)),
    Biggest * SecondBiggest * ThirdBiggest.

%% @doc let all cells tell their neighbors about the smallest known basin cell index
%% all cells in the basin agree.
gossip_until_convergence(AllCells, AdjacencyMap, MinLink) ->
    NewMinLink = gossip(AllCells, AdjacencyMap, MinLink),
    case NewMinLink =:= MinLink of
        true -> MinLink;
        false -> gossip_until_convergence(AllCells, AdjacencyMap, NewMinLink)
    end.

gossip([], _AdjacencyMap, MinLink) -> MinLink;
gossip([Cell | Cells], AdjacencyMap, MinLink0) ->
    Adjacents = maps:get(Cell, AdjacencyMap),
    MinLink = update_min_link(Adjacents, Cell, MinLink0),
    gossip(Cells, AdjacencyMap, MinLink).

update_min_link([], _Cell, MinLink) -> MinLink;
update_min_link([Adjacent|Adjacents], Cell, MinLink0) ->
    CellMinLink = maps:get(Cell, MinLink0),
    AdjacentMinLink = maps:get(Adjacent, MinLink0),
    MinLink = case CellMinLink < AdjacentMinLink of
                  true -> MinLink0#{Adjacent := CellMinLink};
                  false -> MinLink0
              end,
    update_min_link(Adjacents, Cell, MinLink).

adjacency_map({MaxX, MaxY}, IsWall) ->
    maps:from_list([{ {X, Y}, adjacent({X, Y}, {MaxX, MaxY}, IsWall) }
                    || X <- one_to(MaxX), Y <- one_to(MaxY), not IsWall({X, Y})]).

to_wall_map(Grid) -> [[Height =:= 9 || Height <- Row] || Row <- Grid].

adjacent({X, Y}, {MaxX, MaxY}, IsWall) ->
    [
        {AdjacentX, AdjacentY}
        || {AdjacentX, AdjacentY} <- [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}],
           AdjacentX > 0, AdjacentY > 0,
           AdjacentX =< MaxX, AdjacentY =< MaxY,
           not IsWall({AdjacentX, AdjacentY})
    ].

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

% A simple counter I reimplement every once in a while :-/
count(List) -> count(List, #{}).
count([], Counter) ->
    Counter;
count([H | T], Counter) ->
    Increment = fun(N) -> N + 1 end,
    UpdatedCounter = maps:update_with(H, Increment, _Init = 1, Counter),
    count(T, UpdatedCounter).
