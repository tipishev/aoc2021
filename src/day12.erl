-module(day12).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    Graph = parse_graph(Filename),
    find_paths(Graph, start, 'end').

part2(Filename) ->
    parse_graph(Filename).

% Graph extensions

find_paths(Graph, Source, Destination) ->
    NestedPaths = dfs(Graph, Source, Destination, _VisitedSmall = ordsets:new(), []),
    PathStream = lists:flatten(NestedPaths),
    Paths = unpack(PathStream),
    length(Paths).

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

% Depth-First Traversal (DFS)
dfs(_Graph, _Vertex = Destination, Destination, _VisitedSmall, CurrentPath) ->
    CurrentPath ++ [Destination];
dfs(Graph, Vertex, Destination, VisitedSmall0, CurrentPath0) ->
    CurrentPath = CurrentPath0 ++ [Vertex],
    VisitedSmall =
        case is_lower(Vertex) of
            true -> ordsets:add_element(Vertex, VisitedSmall0);
            false -> VisitedSmall0
        end,
    OutNeighbours = out_neighbours(Graph, Vertex),
    VisitableOutNeihbours = ordsets:subtract(OutNeighbours, VisitedSmall0),
    [
        dfs(Graph, Neighbour, Destination, VisitedSmall, CurrentPath)
        || Neighbour <- VisitableOutNeihbours
    ].

out_neighbours(Graph, Vertex) ->
    ordsets:from_list(digraph:out_neighbours(Graph, Vertex)).

add_vertices(Graph, Vertices) ->
    lists:foreach(fun(Vertex) -> digraph:add_vertex(Graph, Vertex) end, Vertices).

add_edges(Graph, Edges) ->
    lists:foreach(fun([Source, Destination]) -> add_edge(Graph, Source, Destination) end, Edges).

add_edge(Graph, Source, Destination) when
    Source =:= start;
    Destination =:= 'end'
->
    digraph:add_edge(Graph, Source, Destination);
add_edge(Graph, Source, Destination) ->
    digraph:add_edge(Graph, Source, Destination),
    digraph:add_edge(Graph, Destination, Source).

edges(Graph) ->
    [
        {Source, Destination}
        || {_, Source, Destination, _} <- [
               digraph:edge(Graph, Edge)
               || Edge <- digraph:edges(Graph)
           ]
    ].

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
    [_Source, _Destination] = [binary_to_atom(Token) || Token <- string:lexemes(Line, "-")].

% herlpers

% is_lower(start) -> not_applicable;
% is_lower('end') -> not_applicable;
is_lower(Atom) ->
    String = atom_to_list(Atom),
    String =:= string:to_lower(String).
