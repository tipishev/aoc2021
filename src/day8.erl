-module(day8).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    PatternsOutputs = parse(File),
    UnfilteredOutputs = [Outputs || {_Patterns, Outputs} <- PatternsOutputs],
    FilteredOutputs = [lists:filter(fun is_1478/1, Output) || Output <- UnfilteredOutputs],
    Counts1478 = [length(FilteredOutput) || FilteredOutput <- FilteredOutputs],
    lists:sum(Counts1478).

% 1
is_1478([_, _]) -> true;
% 7
is_1478([_, _, _]) -> true;
% 4
is_1478([_, _, _, _]) -> true;
% 8
is_1478([_, _, _, _, _, _, _]) -> true;
is_1478(_AnythingElse) -> false.

part2(File) ->
    lists:sum([decode_digits(Patterns, Digits) || {Patterns, Digits} <- parse(File)]).

% Constants

%erlfmt-ignore
segments(1) -> [      c,       f    ];
segments(2) -> [a,    c, d, e,     g];
segments(3) -> [a,    c, d,    f,  g];
segments(4) -> [b,    c, d,    f    ];
segments(5) -> [a, b,    d,    f,  g];
segments(6) -> [a, b,    d, e, f,  g];
segments(7) -> [a,    c,       f    ];
segments(8) -> [a, b, c, d, e, f,  g];
segments(9) -> [a, b, c, d,    f,  g];
segments(0) -> [a, b, c,    e, f,  g].

decode_digits(Patterns, Digits) ->
    SegmentsLookup = deduce_segments_lookup(Patterns),
    DigitsLookup = generate_digits_lookup(SegmentsLookup),
    decode(Digits, DigitsLookup).

% Decoding

decode(EncodedDigits, DigitsLookup) ->
    [Thousands, Hundreds, Tens, Ones] = [
        maps:get(EncodedDigit, DigitsLookup)
        || EncodedDigit <- EncodedDigits
    ],
    Thousands * 1000 + Hundreds * 100 + Tens * 10 + Ones * 1.

% Lookup

generate_digits_lookup(SegmentsLookup) ->
    maps:from_list([
        {encode(segments(Digit), SegmentsLookup), Digit}
        || Digit <- lists:seq(0, 9)
    ]).

deduce_segments_lookup(Patterns) ->
    Lookup = lookup_new(),
    SegmentFrequencies = calculate_segment_frequencies(Patterns),
    LookupA = deduce_a(Lookup, Patterns),
    LookupABEF = deduce_b_e_f(LookupA, SegmentFrequencies),
    LookupABCEF = deduce_c(LookupABEF, Patterns),
    LookupABCDEF = deduce_d(LookupABCEF, Patterns),
    LookupABCDEFG = deduce_g(LookupABCDEF),
    LookupABCDEFG.

lookup_new() ->
    #{
        a => undefined,
        b => undefined,
        c => undefined,
        d => undefined,
        e => undefined,
        f => undefined,
        g => undefined
    }.

deduce_a(Lookup, Patterns) ->
    [One] = by_pattern_length(Patterns, 2),
    [Seven] = by_pattern_length(Patterns, 3),
    [A] = ordsets:subtract(Seven, One),
    Lookup#{a := A}.

deduce_b_e_f(LookupA, Frequencies) ->
    [B] = by_segment_frequency(Frequencies, 6),
    [E] = by_segment_frequency(Frequencies, 4),
    [F] = by_segment_frequency(Frequencies, 9),
    LookupA#{b := B, e := E, f := F}.

deduce_c(LookupABEF = #{f := F}, Patterns) ->
    [One] = by_pattern_length(Patterns, 2),
    [C] = ordsets:subtract(One, [F]),
    LookupABEF#{c := C}.

deduce_d(LookupABCEF = #{b := B, c := C, f := F}, Patterns) ->
    [Four] = by_pattern_length(Patterns, 4),
    BCF = ordsets:from_list([B, C, F]),
    [D] = ordsets:subtract(Four, BCF),
    LookupABCEF#{d := D}.

deduce_g(LookupABCDEF = #{a := A, b := B, c := C, d := D, e := E, f := F}) ->
    ABCDEF = ordsets:from_list([A, B, C, D, E, F]),
    ABCDEFG = [a, b, c, d, e, f, g],
    [G] = ordsets:subtract(ABCDEFG, ABCDEF),
    LookupABCDEF#{g := G}.

encode(Pattern, Lookup) ->
    ordsets:from_list([maps:get(Key, Lookup) || Key <- Pattern]).

% Sets

by_pattern_length(Patterns, Length) ->
    [Pattern || Pattern <- Patterns, length(Pattern) =:= Length].

by_segment_frequency(Frequencies, Frequency) ->
    SegmentCounts = maps:to_list(Frequencies),
    [Segment || {Segment, Count} <- SegmentCounts, Count =:= Frequency].

calculate_segment_frequencies(Patterns) ->
    AllSegments = lists:flatten(Patterns),
    count(AllSegments).

% A simple counter I reimplement every once in a while :-/

count(List) ->
    count(List, #{}).
count([], Counter) ->
    Counter;
count([H | T], Counter) ->
    Increment = fun(N) -> N + 1 end,
    UpdatedCounter = maps:update_with(H, Increment, _Init = 1, Counter),
    count(T, UpdatedCounter).

% Parse

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [parse_patterns_outputs(PatternsOutputs) || PatternsOutputs <- Lines].

parse_patterns_outputs(Line) ->
    [PatternsStr, OutputStr] = string:lexemes(Line, "|"),
    PatternsBins = string:lexemes(PatternsStr, " "),
    OutputsBins = string:lexemes(OutputStr, " "),
    Patterns = [binary_to_atoms_set(PatternsBin) || PatternsBin <- PatternsBins],
    Outputs = [binary_to_atoms_set(OutputBin) || OutputBin <- OutputsBins],
    {Patterns, Outputs}.

binary_to_atoms_set(Bin) ->
    ordsets:from_list([binary_to_atom(<<Byte>>) || <<Byte>> <= Bin]).
