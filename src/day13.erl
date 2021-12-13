-module(day13).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {Dots, _Folds} = parse(Filename),
    {MaxX, MaxY} = find_max(Dots),
    Dimensions = {MaxX + 1, MaxY + 1},
    Sheet = sheet_new(Dimensions),
    sheet_mark(Dots, Sheet).

part2(Filename) ->
    parse(Filename).

% Sheet

sheet_new({MaxX, MaxY}) -> lists:duplicate(MaxY, lists:duplicate(MaxX, o)).

sheet_mark(Dots, Sheet) when is_list(Dots) ->
    lists:foldl(fun sheet_mark/2, Sheet, Dots);
sheet_mark({X0, Y0}, Sheet) ->
    % freaking 0-index
    {X, Y} = {X0 + 1, Y = Y0 + 1},
    Row = lists:nth(Y, Sheet),
    ModifiedRow = lists:sublist(Row, X - 1) ++ [x] ++ lists:nthtail(X, Row),
    lists:sublist(Sheet, Y - 1) ++ [ModifiedRow] ++ lists:nthtail(Y, Sheet).

find_max(Dots) ->
    lists:foldl(fun({X, Y}, {MaxX, MaxY}) -> {max(X, MaxX), max(Y, MaxY)} end, {0, 0}, Dots).

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
