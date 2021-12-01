-module(aoc2021).

%% API exports
-export([main/1]).

%% for running locally
-export([
    day1_1/1,
    day1_2/1
]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([FunStr, InputFile]) ->
    io:format("Running ~s(~p)~n", [FunStr, InputFile]),
    Fun = list_to_atom(FunStr),
    PathToInput = filename:join("inputs", InputFile),
    Result = apply(?MODULE, Fun, [PathToInput]),
    io:format("Result: ~p~n", [Result]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

day1_1(File) ->
    Depths = read_integers(File),
    count_increases(Depths).

day1_2(File) ->
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
%% Santa's little helpers
%%====================================================================

read_integers(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [binary_to_integer(Line) || Line <- Lines].
