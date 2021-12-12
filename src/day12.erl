-module(day12).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    Graph = parse_graph(Filename),
    count_paths_without_repeating_small_caves(Graph).

part2(Filename) ->
    _Graph = parse_graph(Filename).

% Graph extensions

get_label(Graph, Vertex) ->
    {Vertex, Label} = digraph:vertex(Graph, Vertex),
    Label.

count_paths_without_repeating_small_caves(Graph) ->
    NestedPaths = dfs(Graph, start, _VisitedSmall = ordsets:new(), _CurrentPath = []),
    PathStream = lists:flatten(NestedPaths),
    Paths = unpack(PathStream),
    length(Paths).

% Depth-First Traversal (DFS)
dfs(_Graph, 'end', _VisitedSmall, CurrentPath) -> CurrentPath;
dfs(Graph, Vertex, VisitedSmall0, CurrentPath) ->
    % io:format("Label: ~p~n", [get_label(Graph, Vertex)]),
    Label = get_label(Graph, Vertex),
    VisitedSmall = case Label of
                       {small, _Count} -> ordsets:add_element(Vertex, VisitedSmall0);
                       _ -> VisitedSmall0
                   end,
    OutNeighbours = out_neighbours(Graph, Vertex),
    VisitableOutNeihbours = ordsets:subtract(OutNeighbours, VisitedSmall),
    [
        dfs(Graph, Neighbour, VisitedSmall, CurrentPath ++ [Neighbour])
        || Neighbour <- VisitableOutNeihbours
    ].

% %% @doc [start, foo, end, start, foo, bar, end] to [[start, foo, end], [start, foo, bar, end]]
unpack(PathStream) ->
    unpack(PathStream, _CurrentPath = [], _Paths = []).
unpack(_PathStream = [], _CurrentPath = [], Paths) ->
    Paths;
unpack([start | PathStream], _CurrentPath = [], Paths) ->
    unpack(PathStream, [start], Paths);
unpack(['end' | PathStream], CurrentPath, Paths) ->
    CompletePath = lists:reverse(['end' | CurrentPath]),
    unpack(PathStream, [], [CompletePath | Paths]);
unpack([Element | PathStream], CurrentPath, Paths) ->
    unpack(PathStream, [Element | CurrentPath], Paths).

out_neighbours(Graph, Vertex) ->
    ordsets:from_list(digraph:out_neighbours(Graph, Vertex)).

add_vertices(Graph, Vertices) ->
    lists:foreach(fun(Vertex) -> add_vertex(Graph, Vertex) end, Vertices).

% %% @doc add a vertex and label with
% {Type :: small | big | special, Count :: non_neg_integer | not_applicable}
add_vertex(Graph, Vertex) ->
    digraph:add_vertex(Graph, Vertex, label(Vertex)).

label(start) ->
    {special, not_applicable};
label('end') ->
    {special, not_applicable};
label(Vertex) ->
    case detect_case(Vertex) of
        lower -> {small, 0};
        upper -> {big, not_applicable}
    end.

add_edges(Graph, Edges) ->
    lists:foreach(fun([Vertex1, Vertex2]) -> add_edge(Graph, Vertex1, Vertex2) end, Edges).

% unflip incorrect source/destination
add_edge(Graph, 'end', Vertex1) ->
    add_edge(Graph, Vertex1, 'end');
add_edge(Graph, Vertex1, start) ->
    add_edge(Graph, start, Vertex1);

% add just one edge for 'start' and 'end'.
add_edge(Graph, Vertex1, 'end') ->
    digraph:add_edge(Graph, Vertex1, 'end');
add_edge(Graph, start, Vertex2) ->
    digraph:add_edge(Graph, start, Vertex2);

% in general case add both V1->V2 and V2->V1
add_edge(Graph, Vertex1, Vertex2) ->
    digraph:add_edge(Graph, Vertex1, Vertex2),
    digraph:add_edge(Graph, Vertex2, Vertex1).

% Parse

parse_graph(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    Edges = [parse_edge_line(Line) || Line <- Lines],
    Vertices = ordsets:from_list(lists:flatten(Edges)),
    Graph = digraph:new(),
    add_vertices(Graph, Vertices),
    add_edges(Graph, Edges),
    Graph.

parse_edge_line(Line) ->
    [_Vertex1, _Vertex2] = [binary_to_atom(Token) || Token <- string:lexemes(Line, "-")].

% herlpers

detect_case(Atom) ->
    String = atom_to_list(Atom),
    Lower = string:to_lower(String),
    Upper = string:to_upper(String),
    if
        String =:= Lower -> lower;
        String =:= Upper -> upper;
        String =/= Lower andalso String =/= Upper -> mixed
    end.
