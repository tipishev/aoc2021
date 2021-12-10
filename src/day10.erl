-module(day10).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    parse(File).

part2(File) ->
    parse(File).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [[tokenize(<<Char>>) || <<Char>> <= Line] || Line <- Lines].

tokenize(<<"(">>) -> {open, round};
tokenize(<<")">>) -> {close, round};
tokenize(<<"[">>) -> {open, square};
tokenize(<<"]">>) -> {close, square};
tokenize(<<"{">>) -> {open, curly};
tokenize(<<"}">>) -> {close, curly};
tokenize(<<"<">>) -> {open, angly};
tokenize(<<">">>) -> {close, angly}.

score(round) -> 3;
score(square) -> 57;
score(curly) -> 1197;
score(angly) -> 25137.
