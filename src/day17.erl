-module(day17).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {{x, LowX, HighX}, {y, LowY, HighY}} = parse(Filename),
    Target = corners({LowX, HighX}, {LowY, HighY}),
    Grid0 = grid(),
    Grid1 = add(Grid0, {0, 0}, 'S'),
    Grid2 = add_rectangle(Grid1, {LowX, HighX}, {LowY, HighY}, 'T'),
    Grid  = step(Grid2, _Position = {0, 0}, _Velocity={6, 9}, Target),
    draw(Grid).

part2(Filename) ->
    parse(Filename).

%%% Grid

corners({LowX, HighX}, {LowY, HighY}) -> [{X, Y} || X <- [LowX, HighX], Y <- [LowY, HighY]].

is_closer(OldPosition, NewPosition, Targets) when is_list(Targets) ->
    lists:any(fun(Target) -> is_closer(OldPosition, NewPosition, Target) end, Targets);
is_closer(OldPosition,NewPosition, Target) ->
    distance(NewPosition, Target) < distance(OldPosition, Target) + 10.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt((X1-X2)*(X1-X2) + (Y1 - Y2)*(Y1-Y2)).

grid() -> #{}.

add(Grid, {X, Y}, Value) ->
    Grid#{{X,Y} => Value}.

add_rectangle(Grid, {LowX, HighX}, {LowY, HighY}, Value) ->
    Xs = lists:seq(LowX, HighX),
    Ys = lists:seq(LowY, HighY),
    XYs = [{X, Y} || X <- Xs, Y <- Ys],
    lists:foldl(fun({X, Y}, GridAcc) -> add(GridAcc, {X, Y}, Value) end, Grid, XYs).

draw(Grid) ->
    XYs = maps:keys(Grid),
    Xs = [X || {X, _Y} <- XYs],
    Ys = [Y || {_X, Y} <- XYs],
    {LowX, HighX} = min_max(Xs),
    {LowY, HighY} = min_max(Ys),
    draw(Grid, {LowX - 1, HighX + 1}, {LowY - 1, HighY + 1}).


draw(Grid, {LowX, HighX}, {LowY, HighY}) ->
    Xs = lists:seq(LowX, HighX),
    Ys = lists:seq(LowY, HighY),
    String = lists:flatten(
        lists:join("\n", lists:reverse([
            ([atom_to_list(maps:get({X, Y}, Grid, '.')) || X <- Xs])
            || Y <- Ys
        ]))
    ),
    io:format("~s~n", [String]).

step(Grid0, {X0, Y0}, {Dx0, Dy0}, Target) ->
    X = X0 + Dx0,
    Y = Y0 + Dy0,
    case is_closer({X0, Y0}, {X, Y}, Target) of
        true ->
            Dx =
                if
                    Dx0 > 0 -> Dx0 - 1;
                    Dx0 < 0 -> Dx0 + 1;
                    Dx0 =:= 0 -> 0
                end,
            Dy = Dy0 - 1,
            Grid = add(Grid0, {X, Y}, '#'),
            step(Grid, {X, Y}, {Dx, Dy}, Target);
        false -> Grid0
    end.

%%% Parser

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [<<"target">>, <<"area">>, RangeX, RangeY] = string:lexemes(FileContent, " \n:,"),
    {parse_range(RangeX), parse_range(RangeY)}.

parse_range(<<AxisBin:1/binary, "=", Numbers/binary>>) ->
    [LowBin, HighBin] = string:lexemes(Numbers, "."),
    {binary_to_atom(AxisBin), to_int(LowBin), to_int(HighBin)}.

%%% Herlpers

to_int(Bin) ->
    {Int, <<>>} = string:to_integer(Bin),
    Int.

min_max(List) -> {lists:min(List), lists:max(List)}.
