-module(day3).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Binaries = read_binaries(File),
    {Gamma, Epsilon} = gamma_epsilon(Binaries),
    GammaDec = bin_to_dec(Gamma),
    EpsilonDec = bin_to_dec(Epsilon),
    GammaDec * EpsilonDec.

gamma_epsilon(Binaries) ->
    [CountsAcc | CountsTail] = [count(Binary) || Binary <- Binaries],
    ReducedCounts = lists:foldl(fun sum/2, CountsAcc, CountsTail),
    Gamma = [most_common(Count) || Count <- ReducedCounts],
    Epsilon = inverse(Gamma),
    {Gamma, Epsilon}.

count(List) ->
    [increment(Element, {0, 0}) || Element <- List].

increment(0, {CountZero, CountOne}) -> {CountZero + 1, CountOne};
increment(1, {CountZero, CountOne}) -> {CountZero, CountOne + 1}.

bin_to_dec(Bin) ->
    Indices = lists:seq(0, length(Bin) - 1),
    IndexBits = lists:zip(Indices, lists:reverse(Bin)),
    lists:sum([Bit * round(math:pow(2, Index)) || {Index, Bit} <- IndexBits]).

inverse(Bin) when is_list(Bin) -> [inverse(Element) || Element <- Bin];
inverse(0) -> 1;
inverse(1) -> 0.

sum(CountsA, CountsB) when is_list(CountsA) andalso is_list(CountsB) ->
    [sum(AtPosA, AtPosB) || {AtPosA, AtPosB} <- lists:zip(CountsA, CountsB)];
sum({CountZeroA, CountOneA}, {CountZeroB, CountOneB}) ->
    {CountZeroA + CountZeroB, CountOneA + CountOneB}.

most_common({CountZero, CountOne}) when CountZero > CountOne -> 0;
most_common({CountZero, CountOne}) when CountZero < CountOne -> 1;
most_common({Same, Same}) -> 1.

part2(File) ->
    Binaries = read_binaries(File),
    Oxy = oxy(Binaries),
    Scrub = scrub(Binaries),
    Oxy * Scrub.

oxy(Binaries) ->
    oxy(1, Binaries).
oxy(_Pos, [TheOne]) -> bin_to_dec(TheOne);
oxy(Pos, Binaries) ->
    ValuesAtPos = [lists:nth(Pos, Binary) || Binary <- Binaries],
    Mode = mode(ValuesAtPos),
    Filtered = [Binary || Binary <- Binaries, lists:nth(Pos, Binary) =:= Mode],
    oxy(Pos + 1, Filtered).

scrub(Binaries) ->
    scrub(1, Binaries).
scrub(_Pos, [TheOne]) -> bin_to_dec(TheOne);
scrub(Pos, Binaries) ->
    ValuesAtPos = [lists:nth(Pos, Binary) || Binary <- Binaries],
    Mode = inverse(mode(ValuesAtPos)),
    Filtered = [Binary || Binary <- Binaries, lists:nth(Pos, Binary) =:= Mode],
    scrub(Pos + 1, Filtered).

%% @doc finds the most common bit in a binary
mode(Binary) ->
    Sum = lists:sum(Binary),
    Length = length(Binary),
    mode(Sum, Length).

mode(Sum, Length) when Sum >= Length/2 -> 1;
mode(_, _) -> 0.



%%====================================================================
%% Santa's little helpers
%%====================================================================

read_binaries(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [to_integers_list(Line) || Line <- Lines].

to_integers_list(BinInteger) -> [Char - 48 || <<Char>> <= BinInteger].
