-module(day14).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {Template, Rules} = parse(Filename),
    Polymerized = polimerize(Template, Rules, 10),
    Values = maps:values(count(Polymerized)),
    lists:max(Values) - lists:min(Values).

part2(Filename) ->
    {Template, Rules} = parse(Filename),
    Polymerized = polimerize(Template, Rules, 15),
    Values = maps:values(count(Polymerized)),
    lists:max(Values) - lists:min(Values).

insert(Input = [First | InputTail], Rules) ->
    Pairs = to_pairs(Input),
    Insertions = [maps:get(Pair, Rules) || Pair <- Pairs],
    lists:flatten([First | [[Inserted, Original] || {Inserted, Original} <- lists:zip(Insertions, InputTail)]]).

% Generators

polimerize(Template, Rules, Times) ->
    lists:foldl(fun(_Idx, Acc) -> insert(Acc, Rules) end, Template, one_to(Times)).

to_pairs([]) -> [];
to_pairs([_]) -> [];
to_pairs([ A, B | Tail]) -> [{A, B} | to_pairs([B | Tail])].

% Parse

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [Template | RuleLines] = string:lexemes(string:to_lower(binary_to_list(FileContent)), "\n"),
    Rules = [parse_rule(Rule) || Rule <- RuleLines],
    {to_atoms(Template), maps:from_list(Rules)}.

parse_rule(RuleLine) ->
    [From, To] = string:lexemes(string:to_lower(RuleLine), "- >"),
    {list_to_tuple(to_atoms(From)), list_to_atom(To)}.

to_atoms(String) ->
    [list_to_atom([Char]) || Char <- String].

one_to(Value) -> lists:seq(1, Value).

% A simple counter I reimplement every once in a while :-/

count(List) ->
    count(List, #{}).
count([], Counter) ->
    Counter;
count([H | T], Counter) ->
    Increment = fun(N) -> N + 1 end,
    UpdatedCounter = maps:update_with(H, Increment, _Init = 1, Counter),
    count(T, UpdatedCounter).
