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
    Columns = transpose(Binaries),
    Gamma = [most_common_bit(Column) || Column <- Columns],
    Epsilon = inverse(Gamma),
    {Gamma, Epsilon}.

%% stolen from https://stackoverflow.com/a/7855826
transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

bin_to_dec(Bin) ->
    Indices = lists:seq(0, length(Bin) - 1),
    IndexBits = lists:zip(Indices, lists:reverse(Bin)),
    lists:sum([Bit * round(math:pow(2, Index)) || {Index, Bit} <- IndexBits]).

inverse(Bin) when is_list(Bin) -> [inverse(Element) || Element <- Bin];
inverse(0) -> 1;
inverse(1) -> 0.

part2(File) ->
    Binaries = read_binaries(File),
    Oxy = oxy(Binaries),
    Scrub = scrub(Binaries),
    Oxy * Scrub.

oxy(Binaries) -> filter(oxy, Binaries).
scrub(Binaries) -> filter(scrub, Binaries).

filter(oxy, Binaries) -> filter(Binaries, fun most_common_bit/1);
filter(scrub, Binaries) -> filter(Binaries, fun least_common_bit/1);
filter(Binaries, ModeFun) -> filter(1, Binaries, ModeFun).

filter(_Pos, [TheOne], _ModeFun) -> bin_to_dec(TheOne);
filter(Pos, Binaries, ModeFun) ->
    ValuesAtPos = [lists:nth(Pos, Binary) || Binary <- Binaries],
    Mode = ModeFun(ValuesAtPos),
    Filtered = [Binary || Binary <- Binaries, lists:nth(Pos, Binary) =:= Mode],
    filter(Pos + 1, Filtered, ModeFun).

most_common_bit(Binary) ->
    Sum = lists:sum(Binary),
    Length = length(Binary),
    most_common_bit(Sum, Length).
most_common_bit(Sum, Length) when Sum >= Length/2 -> 1;
most_common_bit(_, _) -> 0.

least_common_bit(Binary) -> inverse(most_common_bit(Binary)).

%%====================================================================
%% Santa's little parsers
%%====================================================================

read_binaries(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [to_integers_list(Line) || Line <- Lines].

to_integers_list(BinInteger) -> [Char - 48 || <<Char>> <= BinInteger].
