-module(day4).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    parse(File).

part2(File) ->
    parse(File).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [HeaderStr, BoardsStr] = string:split(FileContent, "\n\n"),
    DrawnNumbers = parse_header(HeaderStr),
    Boards = parse_boards(BoardsStr),
    {DrawnNumbers, Boards}.

parse_boards(BoardsStr) -> parse_boards(BoardsStr, _Boards = []).
parse_boards(BoardsStr, Boards) ->
    case string:find(BoardsStr, "\n\n") of
        nomatch ->
            TheLastBoard = parse_board(BoardsStr),
            lists:reverse([TheLastBoard | Boards]);
        _MoreThanOneBoard ->
            [BoardStr, RemainingBoardStr] = string:split(BoardsStr, "\n\n"),
            Board = parse_board(BoardStr),
            parse_boards(RemainingBoardStr, [Board | Boards])
    end.

parse_board(BoardStr) ->
    Rows = string:lexemes(BoardStr, "\n"),
    [[binary_to_integer(Value)
      || Value <- string:lexemes(Row, " ")] || Row <- Rows].

parse_header(HeaderStr) ->
    DrawnNumbers = string:lexemes(HeaderStr, ","),
    [binary_to_integer(DrawnNumber) || DrawnNumber <- DrawnNumbers].
