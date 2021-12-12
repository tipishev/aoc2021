-module(day12).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    parse(Filename).

part2(Filename) ->
    parse(Filename).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [parse_line(Line) || Line <- Lines].

parse_line(Line) ->
  [Source, Destination] = string:lexemes(Line, "-"),
  {binary_to_atom(Source), binary_to_atom(Destination)}.

