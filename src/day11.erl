-module(day11).

%% API exports
-export([part1/1, part2/1]).

-define(GRID_SIZE, 10).

part1(File) -> count_flashes(parse(File), 100).
part2(File) -> parse(File).

% Grid

% display(Grid) ->
%     chunkify([Val|| {_Pos, Val} <- lists:sort(maps:to_list(Grid))], ?GRID_SIZE).

count_flashes(Grid, NumberOfIterations) ->
    {_FinalGrid, FinalFlashCount} = lists:foldl(
        fun(_IterationIndex, {GridAcc, TotalFlashCount}) ->
            {NewGrid, FlashCount} = step(GridAcc),
            {NewGrid, TotalFlashCount + FlashCount}
        end,
        {_InitialGrid = Grid, _InitialFlashCount = 0},
        one_to(NumberOfIterations)
    ),
    FinalFlashCount.

step(Grid) ->
    NewGrid = flash(increment(Grid)),
    FlashCount = count_zeros(maps:values(NewGrid)),
    {NewGrid, FlashCount}.

flash(Grid) ->
    NewFlashers = find_new_flashers(Grid),
    case NewFlashers of
        [] ->
            After = replace(Grid, f, 0),
            After;
        Flashers ->
            Adjacents = [adjacent(Flasher) || Flasher <- Flashers],
            WithFlashers = mark(Grid, Flashers, f),
            WithIncrementedAdjacents = lists:foldl(fun increment_folder/2, WithFlashers, Adjacents),
            flash(WithIncrementedAdjacents)
    end.

find_new_flashers(Grid) ->
    IsNewFlasher = fun
        % an old flasher
        (_Position, f) -> false;
        (_Position, Level) -> Level > 9
    end,
    ordsets:from_list(maps:keys(maps:filter(IsNewFlasher, Grid))).

adjacent({X, Y}) ->
    [
        {AdjX, AdjY}
        || AdjX <- [X - 1, X, X + 1],
           AdjY <- [Y - 1, Y, Y + 1],
           % exclude self
           {AdjX, AdjY} =/= {X, Y},
           AdjX > 0,
           AdjY > 0,
           AdjX =< ?GRID_SIZE,
           AdjY =< ?GRID_SIZE
    ].

increment(Grid) ->
    increment_folder(maps:keys(Grid), Grid).

increment_folder(Positions, Grid) ->
    Increment = fun
        (_, f) -> f;
        (Position, Value) ->
            case lists:member(Position, Positions) of
                true -> Value + 1;
                false -> Value
            end
    end,
    maps:map(Increment, Grid).

replace(Grid, Before, After) ->
    Replace = fun
        (_Position, Value) when Value =:= Before -> After;
        (_Position, Value) -> Value
    end,
    maps:map(Replace, Grid).

% %% @doc mark certain positions on the grid with a Marker
mark(Grid, Positions, Marker) ->
    Mark = fun(Position, Value) ->
        case lists:member(Position, Positions) of
            true -> Marker;
            false -> Value
        end
    end,
    maps:map(Mark, Grid).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    ListOfIntegerLists = [[binary_to_integer(<<Char>>) || <<Char>> <= L] || L <- Lines],
    index_values(ListOfIntegerLists).

% %% @doc Produces a {X, Y} -> Value map from a list of row values.
index_values(Rows) ->
    EnumeratedRows = enumerate([enumerate(Row) || Row <- Rows]),
    index_values(EnumeratedRows, _Index = #{}).
index_values([], Index) -> Index;
index_values([{RowIndex, Row} | Rows], Index) -> index_values(Rows, index_values_row(RowIndex, Row, Index)).
index_values_row(_RowIndex, [], Index) ->
    Index;
index_values_row(RowIndex, [{ColIndex, Value} | Cols], Index) ->
    index_values_row(RowIndex, Cols, Index#{{RowIndex, ColIndex} => Value}).

% Herlpers

enumerate(List) -> lists:zip(lists:seq(1, length(List)), List).
one_to(Value) -> lists:seq(1, Value).
% print(Term) -> io:format ("~p~n", [Term]).
% chunkify([], _) -> [];
% chunkify(L, N) -> [lists:sublist(L, N) | chunkify(lists:nthtail(N, L), N)].
count_zeros(DeepList) -> length([Zero || Zero <- lists:flatten(DeepList), Zero =:= 0]).
