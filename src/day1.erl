-module(day1).

%% API exports
-export([solve/2]).

solve(part1, File) ->
    Depths = read_integers(File),
    count_increases(Depths);

solve(part2, File) ->
    Depths = read_integers(File),
    As = [_ | Bs0] = [_, _ | Cs0] = Depths,
    Bs = Bs0 ++ [0],
    Cs = Cs0 ++ [0, 0],
    ABCs = lists:zip3(As, Bs, Cs),
    Sums = [A + B + C || {A, B, C} <- ABCs],
    count_increases(Sums).

count_increases(Depths) ->
    {_Last, IncreasesCount} = lists:foldl(fun count_increases/2, {'N/A', 0}, Depths),
    IncreasesCount.
count_increases(Depth, {Previous, IncreasesCount}) when Depth > Previous ->
    {Depth, IncreasesCount + 1};
count_increases(Depth, {Previous, IncreasesCount}) when Depth =< Previous ->
    {Depth, IncreasesCount}.

%%====================================================================
%% Santa's little herlpers
%%====================================================================

read_integers(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [binary_to_integer(Line) || Line <- Lines].
