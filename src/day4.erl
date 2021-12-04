-module(day4).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    {Draws, Boards} = parse(File),
    {ok, Winner, Draw} = first_winner(Draws, Boards),
    sum_board(Winner) * Draw.

first_winner(Draws, Boards) ->
    first_winner(Draws, Boards, _PrevDraw = 'N/A').
first_winner(Draws, Boards, PrevDraw) ->
    case find_winner(Boards) of
        {error, notfound} ->
            [Draw | DrawsTail] = Draws,
            MarkedBoards = mark_boards(Boards, Draw),
            first_winner(DrawsTail, MarkedBoards, Draw);
        {ok, Winner} -> {ok, Winner, PrevDraw}
    end.

find_winner([]) -> {error, notfound};
find_winner([Board | Boards]) ->
    case is_winner(Board) of
        true -> {ok, Board};
        false -> find_winner(Boards)
    end.

part2(File) ->
    {Draws, Boards} = parse(File),
    {ok, LastWinner, LastWinDraw} = last_winner(Draws, Boards),
    sum_board(LastWinner) * LastWinDraw.

last_winner(Draws, Boards) ->
    last_winner(Draws, Boards, _PrevWinner = 'N/A', _PrevWinDraw = 'N/A').

% out of draws
last_winner(_Draws = [], _Boards, PrevWinner, PrevWinDraw) ->
    {ok, PrevWinner, PrevWinDraw};
% out of boards
last_winner(_Draws, _Boards = [], PrevWinner, PrevWinDraw) ->
    {ok, PrevWinner, PrevWinDraw};
last_winner(Draws, Boards, PrevWinner, PrevWinDraw) ->
    [Draw | DrawsTail] = Draws,
    MarkedBoards = mark_boards(Boards, Draw),
    {Winner, Stayers} = winner_stayers(MarkedBoards),
    case {Winner, Stayers} of
        % recursive case captchures empty Stayers, too
        {'N/A', Stayers} ->
            last_winner(DrawsTail, Stayers, PrevWinner, PrevWinDraw);
        {Winner, Stayers} ->
            last_winner(DrawsTail, Stayers, Winner, Draw)
    end.

winner_stayers(MarkedBoards) ->
    lists:foldl(fun winners_stayers_folder/2, {'N/A', []}, MarkedBoards).

winners_stayers_folder(MarkedBoard, {PrevWinner, Stayers}) ->
    case is_winner(MarkedBoard) of
        true -> {MarkedBoard, Stayers};
        false -> {PrevWinner, [MarkedBoard | Stayers]}
    end.

%%% Board Logic

%%% Mark
mark_boards(Boards, N) ->
    [ mark_board(Board, N) || Board <- Boards ].
mark_board(Board, N) ->
    [ mark_row(Row, N) || Row <- Board ].
mark_row(Row, N) ->
    [ mark_cell(Cell, N) || Cell <- Row].
mark_cell(N, N) -> x;
mark_cell(Cell, _N) -> Cell.

%%% Sum
sum_board(Board) -> lists:sum([sum_row(Row) || Row <- Board]).
sum_row(Row) -> lists:sum([Cell || Cell <- Row, Cell =/= x]).

%%% Winner-check
is_winner(Board = Rows) ->
    Columns = transpose(Board),
    is_winner_rows(Rows) orelse is_winner_rows(Columns).

is_winner_rows([]) -> false;
is_winner_rows([Row | RowsTail]) ->
    case is_winner_row(Row) of
        true -> true;
        false -> is_winner_rows(RowsTail)
    end.

is_winner_row([x, x, x, x, x]) -> true;
is_winner_row([_, _, _, _, _]) -> false.

%% TODO deduplicate with day3 into utils
%% stolen from https://stackoverflow.com/a/7855826
transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

%%% Parser

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
