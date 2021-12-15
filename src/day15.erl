-module(day15).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {MaxX, MaxY, Grid} = parse(Filename),
    GetAdjacents = make_adjacent_fun(MaxX, MaxY),
    Graph = build_graph(Grid, GetAdjacents),
    dijkstra(Graph, _Source = {1, 1}).

part2(Filename) -> parse(Filename).

% %% @doc https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
dijkstra(Graph, Source) ->
    Set = fun(Vertex, Value) -> digraph:add_vertex(Graph, Vertex, Value) end,
    Get = fun(Vertex) -> digraph:vertex(Graph, Vertex) end,
    Vertices = digraph:vertices(Graph),
    Q = ordsets:from_list(Vertices),
    [Set(Vertex, #{dist => infinity, prev => undefined})
     || Vertex <- Vertices],
    Set(Source, #{dist => 0, prev => undefined}),
    update(Q, Graph).

update([], Graph) -> Graph;
update([Vertex | Vertices], Graph) ->
    Neighbours = digraph:out_neighbours(Graph, Vertex),


% edges(Graph) ->
%     Edges = digraph:edges(Graph),
%     [digraph:edge(Graph, Edge) || Edge <- Edges].

build_graph(Grid, GetAdjacents) ->
    Cells = maps:keys(Grid),
    Graph = digraph:new(),
    % add vertices
    [digraph:add_vertex(Graph, Cell) || Cell <- Cells],
    % add edges
    [
        [
            digraph:add_edge(Graph, Cell, Adjacent, maps:get(Adjacent, Grid))
            || Adjacent <- GetAdjacents(Cell)
        ]
        || Cell <- Cells
    ],
    Graph.

make_adjacent_fun(MaxX, MaxY) -> fun({X, Y}) -> adjacent({MaxX, MaxY}, {X, Y}) end.
adjacent({MaxX, MaxY}, {X, Y}) ->
    [
        {AdjX, AdjY}
        || {AdjX, AdjY} <- [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}],
           % exclude self
           {AdjX, AdjY} =/= {X, Y},
           AdjX > 0,
           AdjY > 0,
           AdjX =< MaxX,
           AdjY =< MaxY
    ].

%%% Parse

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    ListOfIntegerLists = [[binary_to_integer(<<Char>>) || <<Char>> <= L] || L <- Lines],
    Grid = index_values(ListOfIntegerLists),
    MaxX = string:length(lists:nth(1, Lines)),
    MaxY = length(Lines),
    {MaxX, MaxY, Grid}.

% %% @doc Produces a {X, Y} -> Value map from a list of row values.
index_values(Rows) ->
    EnumeratedRows = enumerate([enumerate(Row) || Row <- Rows]),
    index_values(EnumeratedRows, _Index = #{}).
index_values([], Index) -> Index;
index_values([{RowIndex, Row} | Rows], Index) -> index_values(Rows, index_values_row(RowIndex, Row, Index)).
index_values_row(_RowIndex, [], Index) ->
    Index;
index_values_row(RowIndex, [{ColIndex, Value} | Cols], Index) ->
    index_values_row(RowIndex, Cols, Index#{{RowIndex, ColIndex} => Value}).

% Herlpers

enumerate(List) -> lists:zip(lists:seq(1, length(List)), List).
