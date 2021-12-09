-module(day9).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Rows = read_integer_lists(File),
    Index = index(Rows),
    Index.

part2(File) ->
    read_integer_lists(File).

index(Rows) ->
    EnumeratedRows = enumerate([enumerate(Row) || Row <- Rows]),
    index(EnumeratedRows, _Index = #{}).

index([], Index) -> Index;
index([{RowIndex, Row} | Rows], Index) ->
    index(Rows, index_row(RowIndex, Row, Index)).

index_row(_RowIndex, [], Index) -> Index;
index_row(RowIndex, [{ColIndex, Value} | Cols], Index) ->
    index_row(RowIndex, Cols, Index#{{RowIndex, ColIndex} => Value}).

read_integer_lists(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [to_integers_list(Line) || Line <- Lines].

to_integers_list(BinInteger) -> [Char - 48 || <<Char>> <= BinInteger].

% Python, I miss you sometimes :'-[
enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).
