-module(day8).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    PatternsOutputs = parse(File),
    UnfilteredOutputs = [Outputs || {_Patterns, Outputs} <- PatternsOutputs],
    FilteredOutputs = [lists:filter(fun is_1478/1, Output) || Output <- UnfilteredOutputs],
    Counts1478 = [length(FilteredOutput) || FilteredOutput <- FilteredOutputs],
    lists:sum(Counts1478).

is_1478([_, _]) -> true;  % 1
is_1478([_, _, _]) -> true;  % 7
is_1478([_, _, _, _]) -> true;  % 4
is_1478([_, _, _, _, _, _, _]) -> true;  % 8
is_1478(_AnythingElse) -> false.

part2(File) ->
    parse(File).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [parse_patterns_outputs(PatternsOutputs) || PatternsOutputs <- Lines].

parse_patterns_outputs(Line) ->
    [PatternsStr, OutputStr] = string:lexemes(Line, "|"),
    PatternsBins = string:lexemes(PatternsStr, " "),
    OutputsBins = string:lexemes(OutputStr, " "),
    Patterns = [binary_to_atoms(PatternsBin) || PatternsBin <- PatternsBins],
    Outputs = [binary_to_atoms(OutputBin) || OutputBin <- OutputsBins],
    {Patterns, Outputs}.

binary_to_atoms(Bin) ->
    [binary_to_atom(<<Byte>>) || <<Byte>> <= Bin].
