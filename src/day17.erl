-module(day17).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {{x, LowX, HighX}, {y, LowY, HighY}} = parse(Filename),
    Grid0 = grid(),
    Grid1 = add(Grid0, {0, 0}, 'S'),
    Grid2 = add(Grid1, {1, 0}, x),
    Grid3 = add(Grid2, {0, 1}, y),
    Grid = add_rectangle(Grid3, {LowX, HighX}, {LowY, HighY}, 'T'),
    draw(Grid, {min(0, LowX), HighX + 1}, {min(0, LowY - 1), HighY + 8}).

part2(Filename) ->
    parse(Filename).

%%% Grid

grid() -> #{}.

add(Grid, {X, Y}, Value) ->
    Grid#{{X,Y} => Value}.

add_rectangle(Grid, {LowX, HighX}, {LowY, HighY}, Value) ->
    Xs = lists:seq(LowX, HighX),
    Ys = lists:seq(LowY, HighY),
    XYs = [{X, Y} || X <- Xs, Y <- Ys],
    lists:foldl(fun({X, Y}, GridAcc) -> add(GridAcc, {X, Y}, Value) end, Grid, XYs).

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


%%% Parser

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [<<"target">>, <<"area">>, RangeX, RangeY] = string:lexemes(FileContent, " \n:,"),
    {parse_range(RangeX), parse_range(RangeY)}.

parse_range(<<AxisBin:1/binary, "=", Numbers/binary>>) ->
    [LowBin, HighBin] = string:lexemes(Numbers, "."),
    {binary_to_atom(AxisBin), to_int(LowBin), to_int(HighBin)}.

to_int(Bin) ->
    {Int, <<>>} = string:to_integer(Bin),
    Int.

