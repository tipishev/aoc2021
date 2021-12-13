-module(day13).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {Dots, [Fold | _Folds]} = parse(Filename),
    {MaxX, MaxY} = find_max(Dots),
    Dimensions = {MaxX + 1, MaxY + 1},
    Sheet = sheet_new(Dimensions),
    MarkedSheet = mark(Dots, Sheet),
    % FoldedSheet = fold(MarkedSheet, Fold),
    {Top, Bottom} = tear(MarkedSheet, 7),
    show(Top),
    show(Bottom).


part2(Filename) ->
    parse(Filename).

% Sheet

% fold(Sheet, {y, Y}) ->
%     % fold_mark_row(Sheet, Y).
%     [Top, Bottom] = tear(Sheet, Y).

tear(Sheet, Y0) ->
    Y = Y0 + 1,
    Top = lists:sublist(Sheet, Y - 1),
    Bottom = lists:nthtail(Y, Sheet),
    {Top, Bottom}.

fold_mark_row(Sheet, Y0) ->
    Y = Y0 + 1,
    Row = lists:nth(Y, Sheet),
    PlusRow = [ Dash || Dash <- lists:duplicate(length(Row), '+')],
    replace(Y, PlusRow, Sheet).

show(Sheet) ->
    String = lists:flatten([ [ atom_to_list(Cell) || Cell <- Line] ++ "\n" || Line <- Sheet ]),
    io:format("~s~n", [String]).

sheet_new({MaxX, MaxY}) -> lists:duplicate(MaxY, lists:duplicate(MaxX, '.')).

mark(Dots, Sheet) when is_list(Dots) ->
    lists:foldl(fun mark/2, Sheet, Dots);
mark({X0, Y0}, Sheet) ->
    % freaking 0-index
    {X, Y} = {X0 + 1, Y = Y0 + 1},
    Row = lists:nth(Y, Sheet),
    ModifiedRow = replace(X, '#', Row),
    replace(Y, ModifiedRow, Sheet).

find_max(Dots) ->
    lists:foldl(fun({X, Y}, {MaxX, MaxY}) -> {max(X, MaxX), max(Y, MaxY)} end, {0, 0}, Dots).

% Utils

replace(N, Value, List) -> lists:sublist(List, N - 1) ++ [Value] ++ lists:nthtail(N, List).


%% stolen from https://stackoverflow.com/a/7855826
transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

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
