-module(template).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    parse(Filename).

part2(Filename) ->
    parse(Filename).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    Lines.
