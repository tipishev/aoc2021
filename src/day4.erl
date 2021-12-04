-module(day4).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    {DrawnNumbers, Board} = parse(File).

part2(File) ->
    parse(File).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [StrHeader, StrBoard] = string:split(FileContent, "\n\n"),
    DrawnNumbers = parse_header(StrHeader),
    Boards = parse_boards(StrBoard),
    {DrawnNumbers, Boards}.

parse_header(StrHeader) ->
    DrawnNumbers = string:lexemes(StrHeader, ","),
    [binary_to_integer(DrawnNumber) || DrawnNumber <- DrawnNumbers].

parse_boards(StrBoard) -> parse_boards(StrBoard, _Boards = []).
parse_boards(StrBoard, Boards) ->
    case string:find(StrBoard, "\n\n") of
        nomatch ->
            TheLastBoard = parse_board(StrBoard),
            lists:reverse([TheLastBoard | Boards]);
        _MoreThanOneBoard ->
            [BoardStr, RemainingBoardStr] = string:split(StrBoard, "\n\n"),
            Board = parse_board(BoardStr),
            parse_boards(RemainingBoardStr, [Board | Boards])
    end.

parse_board(BoardStr) ->
    Rows = string:lexemes(BoardStr, "\n"),
    [[binary_to_integer(Value)
      || Value <- string:lexemes(Row, " ")] || Row <- Rows].
