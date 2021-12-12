-module(day12).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    Graph = parse_graph(Filename),
    digraph:edges(Graph).

part2(Filename) ->
    parse_graph(Filename).

% Graph extensions

add_vertices(Graph, Vertices) ->
    lists:foreach(fun(Vertex) -> digraph:add_vertex(Graph, Vertex) end, Vertices).

add_edges(Graph, Edges) ->
    lists:foreach(fun([Source, Destination]) ->
                          digraph:add_edge(Graph, Source, Destination)
                          % TODO add a Destination -> Source edge, too?
                  end,
                  Edges).

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
  [_Src, _Dst] = [binary_to_atom(Token) || Token <- string:lexemes(Line, "-")].

