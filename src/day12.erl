-module(day12).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    Graph = parse_graph(Filename),
    find_paths_without_repeating_small_caves(Graph).

part2(Filename) ->
    Graph = parse_graph(Filename),
    find_paths_with_at_most_one_repeating_small_cave(Graph).

% Graph extensions

find_paths_without_repeating_small_caves(Graph) ->
    NestedPaths = dfs(Graph, start, _VisitedSmall = ordsets:new(), _CurrentPath = []),
    PathStream = lists:flatten(NestedPaths),
    Paths = unpack(PathStream),
    length(Paths).

find_paths_with_at_most_one_repeating_small_cave(Graph) ->
    VisitedSmall = ordsets:new(),
    NestedPaths = dfs2(Graph, start, VisitedSmall, _SecondSmall = undefined, _CurrentPath = []),
    PathStream = lists:flatten(NestedPaths),
    Paths = unpack(PathStream),
    io:format("~p~n", [Paths]),
    length(Paths).

% Depth-First Traversal (DFS)
dfs(_Graph, 'end', _VisitedSmall, CurrentPath) -> CurrentPath;
dfs(Graph, Vertex, VisitedSmall0, CurrentPath) ->
    VisitedSmall =
        case is_lower(Vertex) of
            true -> ordsets:add_element(Vertex, VisitedSmall0);
            false -> VisitedSmall0;
            not_applicable -> VisitedSmall0
        end,
    OutNeighbours = out_neighbours(Graph, Vertex),
    VisitableOutNeihbours = ordsets:subtract(OutNeighbours, VisitedSmall),
    [
        dfs(Graph, Neighbour, VisitedSmall, CurrentPath ++ [Neighbour])
        || Neighbour <- VisitableOutNeihbours
    ].

% DFS for part 2

% arrived to the 'end'
dfs2(_Graph, 'end', _VisitedSmall, _SecondSmall, CurrentPath) ->
    CurrentPath ++ ['end'];

% a small cave has not been visited twice yet
dfs2(Graph, Vertex, VisitedSmall0, _SecondSmall=undefined, CurrentPath0) ->
    CurrentPath = CurrentPath0 ++ [Vertex],
    {VisitedSmall, SecondSmall} =
        case is_lower(Vertex) of
            true ->
                case ordsets:is_element(Vertex, VisitedSmall0) of
                    % this cave is small and has been visited, so mark it as second small
                    true -> {VisitedSmall0, Vertex};
                    % this cave has not been visited befor, second small stays undefined
                    false -> {ordsets:add_element(Vertex, VisitedSmall0), undefined}
                end;
            false ->
                {VisitedSmall0, undefined}
        end,
    % anything goes really
    OutNeighbours = out_neighbours(Graph, Vertex),
    VisitableOutNeihbours = ordsets:subtract(OutNeighbours, ['start']),
    [
        dfs2(Graph, Neighbour, VisitedSmall, SecondSmall, CurrentPath)
        || Neighbour <- VisitableOutNeihbours
    ];
% a small cave has been visited twice
dfs2(Graph, Vertex, VisitedSmall0, SecondSmall, CurrentPath0) ->
    CurrentPath = CurrentPath0 ++ [Vertex],
    VisitedSmall = case is_lower(Vertex) of
        true -> ordsets:add_element(Vertex, VisitedSmall0);
        false -> VisitedSmall0
    end,
    OutNeighbours = out_neighbours(Graph, Vertex),
    VisitableOutNeihbours = ordsets:subtract(OutNeighbours, VisitedSmall),
    [
        dfs2(Graph, Neighbour, VisitedSmall, SecondSmall, CurrentPath)
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
    lists:foreach(fun(Vertex) -> digraph:add_vertex(Graph, Vertex) end, Vertices).

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

is_lower(start) -> not_applicable;
is_lower('end') -> not_applicable;
is_lower(Atom) ->
    String = atom_to_list(Atom),
    String =:= string:to_lower(String).
