-module(day15).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {MaxX, MaxY, Grid} = parse(Filename),
    GetAdjacents = make_adjacent_fun(MaxX, MaxY),
    Graph0 = build_graph(Grid, GetAdjacents),
    Graph = dijkstra(Graph0, _Source = {1, 1}),
    #{dist := Dist}  = get_vertex_label(Graph, {MaxX, MaxY}),
    Dist.

part2(Filename) -> parse(Filename).

% %% @doc https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
dijkstra(Graph, Source) ->
    Vertices = digraph:vertices(Graph),
    Q = Vertices,
    [set_vertex_label(Graph, Vertex, #{dist => infinity, prev => undefined})
     || Vertex <- Vertices],
    set_vertex_label(Graph, Source, #{dist => 0, prev => undefined}),
    get_vertex_label(Graph, Source),
    loop(Graph, Q).

loop(Graph, []) -> Graph;
loop(Graph, Q0) ->
    U = find_min_dist_vertex(Graph, Q0),
    Q = lists:delete(U, Q0),
    DistU = get_dist(Graph, U),
    NeighboursU = digraph:out_neighbours(Graph, U),
    [begin
         Alt = DistU + get_edge_label(Graph, {U, V}),
         DistV = get_dist(Graph, V),
         case Alt < DistV of
             true -> set_vertex_label(Graph, V, #{dist => Alt, prev => U});
             false -> ok
         end
     end || V <- NeighboursU],
    loop(Graph, Q).


    % Q1 = lists:delete(U, Q0),
    % NeighboursOfU = digraph:out_neighbours(Graph, U),

get_edge_label(Graph, {Vertex1, Vertex2}) ->
    Edges = [digraph:edge(Graph, Edge) || Edge <- digraph:edges(Graph, Vertex1)],
    [Label] = ([
        Label
        || {_, VertexFrom, VertexTo, Label} <- Edges,
           VertexFrom =:= Vertex1,
           VertexTo =:= Vertex2
    ]),
    Label.



set_vertex_label(Graph, Vertex, Value) ->
    digraph:add_vertex(Graph, Vertex, Value).

get_dist(Graph, Vertex) ->
    #{dist := Dist} = get_vertex_label(Graph, Vertex),
    Dist.

get_vertex_label(Graph, Vertex) ->
    {Vertex, Label} = digraph:vertex(Graph, Vertex),
    Label.

find_min_dist_vertex(Graph, Vertices) ->
    {_MinDist, Vertex} = hd(lists:sort([
        begin
            Dist = get_dist(Graph, Vertex),
            {Dist, Vertex}
        end
        || Vertex <- Vertices
    ])),
    Vertex.

% update([], Graph) -> Graph;
% update([Vertex | Vertices], Graph) ->
%     Neighbours = digraph:out_neighbours(Graph, Vertex),


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
