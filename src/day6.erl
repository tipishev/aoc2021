-module(day6).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Timers = parse_int_array(File),
    Counter = to_counter(Timers),
    lists:sum(advance(Counter, 80)).

part2(File) ->
    Timers = parse_int_array(File),
    Counter = to_counter(Timers),
    lists:sum(advance(Counter, 256)).

advance(Counter, Times) ->
    lists:foldl(fun(_Idx, AccCounter) -> advance(AccCounter) end, Counter, lists:seq(1, Times)).

%         0     1    2       3     4     5         6          7      8
advance({Zero, One, Two,   Three, Four, Five, Six,          Seven, Eight}) ->
        {One,  Two, Three, Four,  Five, Six,  Seven + Zero, Eight, Zero }.

to_counter(Integers) ->
    to_counter(Integers, [0, 0, 0, 0, 0, 0, 0, 0, 0]).
to_counter([], Counter) -> list_to_tuple(Counter);
to_counter([H|T], Counter) ->
    % because Erlang lists are 1-indexed
    N = H + 1,
    OldValue = lists:nth(N, Counter),
    to_counter(T, setnth(N, Counter, OldValue + 1)).

% stolen from https://stackoverflow.com/a/4781219
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

parse_int_array(Filename) ->
    {ok, CsvStrIntegers} = file:read_file(Filename),
    StrIntegers = string:lexemes(CsvStrIntegers, ",\n"),
    _Integers = [binary_to_integer(StrInteger) || StrInteger <- StrIntegers].
