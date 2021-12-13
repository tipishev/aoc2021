-module(day13).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    parse(Filename).

part2(Filename) ->
    parse(Filename).

% Parser

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    TokenizedLines = [tokenize(Line) || Line <- Lines],
    Dots = [{X, Y} || {dot, {X, Y}} <- TokenizedLines],
    Folds = [{Axis, N} || {fold, {Axis, N}} <- TokenizedLines],
    {Dots, Folds}.

tokenize(Line) ->
    case string:lexemes(Line, ", =") of
        [StrX, StrY] -> {dot, {binary_to_integer(StrX), binary_to_integer(StrY)}};
        [<<"fold">>, <<"along">>, Axis, N] -> {fold, {binary_to_atom(Axis), binary_to_integer(N)}}
    end.
