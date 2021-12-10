-module(day11).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    parse(File).

part2(File) ->
    parse(File).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    Lines.
