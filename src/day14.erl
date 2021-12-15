-module(day14).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {Template, Insertions} = parse(Filename),
    solve(Template, Insertions, 10).

part2(Filename) ->
    {Template, Insertions} = parse(Filename),
    solve(Template, Insertions, 40).

solve(Template, Insertions, Steps) ->
    PairFrequencies0 = to_pair_frequencies(Template),
    SymbolFrequencies0 = count(Template),
    PairGenerator = to_generator(Insertions),
    SymbolGenerator = maps:from_list(Insertions),

    Update = fun({SymbolFrequencies, PairFrequencies}) ->
        update(
            SymbolFrequencies,
            PairFrequencies,
            SymbolGenerator,
            PairGenerator
        )
    end,
    {SymbolFrequencies, _PairFrequencies} = lists:foldl(
      fun(_Idx, Acc) -> Update(Acc) end,
      {SymbolFrequencies0, PairFrequencies0},
      lists:seq(1, Steps)
    ),
    max_min_diff(SymbolFrequencies).

max_min_diff(SymbolFrequencies) ->
    SortedCounts = lists:sort([Count || {_Symbol, Count} <- maps:to_list(SymbolFrequencies)]),
    lists:last(SortedCounts) - hd(SortedCounts).

update(SymbolFrequencies0, PairFrequencies0, SymbolGenerator, PairGenerator) ->

    % update pair frequencies
    PairFrequencyDeltas = lists:flatten([
        begin
            [NewPair1, NewPair2] = maps:get(Pair, PairGenerator),
            [{NewPair1, Multiplier}, {NewPair2, Multiplier}]
        end
        || {Pair, Multiplier} <- maps:to_list(PairFrequencies0)
    ]),

    PairFrequencies = lists:foldl(
        fun({Pair, Multiplier}, Acc) ->
            Fun = fun(CurrentCount) -> CurrentCount + Multiplier end,
            maps:update_with(Pair, Fun, Multiplier, Acc)
        end,
        #{},
        PairFrequencyDeltas
    ),

    % update symbol frequencies
    SymbolFrequencyDeltas = [{maps:get(Pair, SymbolGenerator), Multiplier} ||
               {Pair, Multiplier} <- maps:to_list(PairFrequencies0)],
    SymbolFrequencies = lists:foldl(
      fun({Symbol, Multiplier}, Acc) ->
          Fun = fun(CurrentCount) -> CurrentCount + Multiplier end,
          maps:update_with(Symbol, Fun, Multiplier, Acc)
      end,
      SymbolFrequencies0,
      SymbolFrequencyDeltas
    ),
    {SymbolFrequencies, PairFrequencies}.



to_generator(Insertions) ->
    maps:from_list([{[A, C], [[A, B], [B, C]]} || {[A, C], B} <- Insertions]).

to_pair_frequencies(Template) ->
    count(to_pairs(Template)).

% Parse

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [Template | InsertionLines] = string:lexemes(string:to_lower(binary_to_list(FileContent)), "\n"),
    Insertions = [parse_insertion(InsertionLine) || InsertionLine <- InsertionLines],
    {to_atoms(Template), Insertions}.

parse_insertion(InsertionLine) ->
    [From, To] = string:lexemes(string:to_lower(InsertionLine), "- >"),
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
