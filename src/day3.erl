-module(day3).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Lists = read_strings(File),
    [Acc | Tail] = [count(List) || List <- Lists],
    ReducedCounts = lists:foldl(fun sum/2, Acc, Tail),
    Gamma = [most_common(Count) || Count <- ReducedCounts],
    Epsilon = inverse(Gamma),
    GammaDec = bin_to_dec(Gamma),
    EpsilonDec = bin_to_dec(Epsilon),
    GammaDec * EpsilonDec.

count(List) ->
    [increment(Element, {0, 0}) || Element <- List].

increment(0, {CountZero, CountOne}) -> {CountZero + 1, CountOne};
increment(1, {CountZero, CountOne}) -> {CountZero, CountOne + 1}.

bin_to_dec(Bin) ->
    Indices = lists:seq(0, length(Bin) - 1),
    IndexBits = lists:zip(Indices, lists:reverse(Bin)),
    lists:sum([Bit * math:pow(2, Index) || {Index, Bit} <- IndexBits]).

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
    read_binary_integers(File).

%%====================================================================
%% Santa's little helpers
%%====================================================================
%%
read_strings(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    io:format("Loaded ~p lines.~n", [length(Lines)]),
    CharLists = [
        begin
            BinString = binary_to_list(Line),
            [X - 48 || X <- BinString]
        end
        || Line <- Lines
    ],
    CharLists.

read_binary_integers(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    BinLines = string:lexemes(FileContent, "\n"),
    StrLines = [binary_to_list(BinLine) || BinLine <- BinLines],
    [list_to_integer(StrLine, 2) || StrLine <- StrLines].
