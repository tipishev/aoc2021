-module(aoc2021).

%% API exports
-export([main/1]).

%% for running locally
-export([
    day1_1/1
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
    CountIncreasesFun = fun(Depth, {Previous, IncreaseCount}) ->
                  case Depth > Previous of
                      true -> {Depth, IncreaseCount + 1};
                      false -> {Depth, IncreaseCount}
                  end
          end,
    {_Last, IncreasesCount} = lists:foldl(CountIncreasesFun, {'N/A', 0}, Depths),
    IncreasesCount.

read_integers(File) ->
    {ok, BigString} = file:read_file(File),
    Lines = string:lexemes(BigString, "\n"),
    [binary_to_integer(Line) || Line <- Lines].
