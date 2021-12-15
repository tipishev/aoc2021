-module(day14).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {Template, Lookup} = parse(Filename),
    Generator = lookup_to_generator(Lookup),
    SymbolCounter = polymerize(Template, Generator, 10),
    max_min_diff(SymbolCounter).

part2(Filename) ->
    {Template, Lookup} = parse(Filename),
    Generator = lookup_to_generator(Lookup),
    _Polymerized = polymerize(Template, Generator, 23).
    % Values = maps:values(count(Polymerized)),
    % lists:max(Values) - lists:min(Values).

polymerize(Template, Generator, Steps) ->
    Pairs = to_pairs(Template),
    PairFrequencies = count(Pairs),
    % Updated = update(PairFrequencies, Generator),
    Updated = lists:foldl(
                fun(_Idx, FrequencyAcc) -> update(FrequencyAcc, Generator) end,
                PairFrequencies,
                lists:seq(1, Steps)),
    frequency_to_symbol_counter(Updated).

update(PairFrequencies, Generator) ->
    Foo = [{_Multiplier = Frequency,
      _GeneratedPairs = maps:get(Pair, Generator)}
     || {Pair, Frequency} <- maps:to_list(PairFrequencies)],
    Deltas = lists:flatten([[{Pair1, Multiplier}, {Pair2, Multiplier}]
              || {Multiplier, [Pair1, Pair2]} <- Foo]),
    lists:foldl(fun frequency_folder/2, #{}, Deltas).

frequency_folder({Key, Increase}, Acc) ->
    Fun = fun(Before) -> Before + Increase end,
    maps:update_with(Key, Fun, Increase, Acc).


lookup_to_generator(Lookup) ->
    PairInsertion = maps:to_list(Lookup),
    PairPairs = [{[First, Second], [[First, Insertion], [Insertion, Second]]}
                 || {[First, Second], Insertion} <- PairInsertion ],
    maps:from_list(PairPairs).

frequency_to_symbol_counter(FrequencyCounts) ->
    Deltas = lists:flatten([ [{Symbol1, Count}, {Symbol2, Count}]
    || {[Symbol1, Symbol2], Count} <- maps:to_list(FrequencyCounts)]),
    lists:foldl(fun frequency_folder/2, #{}, Deltas).


max_min_diff(SymbolCounter) ->
    SymbolCounts = maps:to_list(SymbolCounter),
    CountSymbols = lists:sort([{Count, Symbol} || {Symbol, Count} <- SymbolCounts]),
    {MinCount, _MinSymbol} = first(CountSymbols),
    {MaxCount, _MaxSymbol} = last(CountSymbols),
    MaxCount - MinCount.

% Parse

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [Template | RuleLines] = string:lexemes(string:to_lower(binary_to_list(FileContent)), "\n"),
    Lookup = [parse_rule(Rule) || Rule <- RuleLines],
    {to_atoms(Template), maps:from_list(Lookup)}.

parse_rule(RuleLine) ->
    [From, To] = string:lexemes(string:to_lower(RuleLine), "- >"),
    {to_atoms(From), list_to_atom(To)}.

to_atoms(String) ->
    [list_to_atom([Char]) || Char <- String].

to_pairs([]) -> [];
to_pairs([_]) -> [];
to_pairs([ A, B | Tail]) -> [[A, B] | to_pairs([B | Tail])].

% A simple counter I reimplement every once in a while :-/
count(List) ->
    count(List, #{}).
count([], Counter) ->
    Counter;
count([H | T], Counter) ->
    Increment = fun(N) -> N + 1 end,
    UpdatedCounter = maps:update_with(H, Increment, _Init = 1, Counter),
    count(T, UpdatedCounter).

last(List) -> lists:last(List).
first(List) -> hd(List).
