-module(day14).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {Template, Lookup} = parse(Filename),
    Polymerized = polimerize(Template, Lookup, 10),
    max_min_diff(Polymerized).

part2(Filename) ->
    {Template, Lookup} = parse(Filename),
    Polymerized = polimerize(Template, Lookup, 10),
    Values = maps:values(count(Polymerized)),
    lists:max(Values) - lists:min(Values).

% Internal

max_min_diff(Chain) ->
    Values = maps:values(count(Chain)),
    lists:max(Values) - lists:min(Values).

polimerize(Chain, _Lookup, 0) -> Chain;
polimerize(Chain, Lookup, Times) ->
    {NewChain, NewLookup} = insert(Chain, Lookup),
    polimerize(NewChain, NewLookup, Times - 1).

insert([], Lookup) -> {[], Lookup};
insert([A], Lookup) -> {[A], Lookup};
insert([A, B], Lookup) -> {[A, maps:get([A, B], Lookup), B], Lookup};
insert(Chain, Lookup) ->
    % io:format("Chain: ~p~n", [Chain]),
    % io:format("Lookup: ~p~n", [Lookup]),
    {Left, Right} = half(Chain),
    Middle = maps:get([last(Left), first(Right)], Lookup),
    {NewLeft, _NewLeftLookup} = insert(Left, Lookup),
    {NewRight, _NewRightLookup} = insert(Right, Lookup),
    % io:format("NewLeft: ~p~nNewRight:~p~n", [NewLeft, NewRight]),
    NewChain = NewLeft ++ [Middle] ++ NewRight,
    % NewLookup = maps:merge(NewLeftLookup, NewRightLookup),
    NewLookup = Lookup#{Chain => NewChain},
    {NewChain, NewLookup}.

half(Chain) ->
    MiddleIndex = length(Chain) div 2,
    Left = lists:sublist(Chain, MiddleIndex),
    Right = lists:nthtail(MiddleIndex, Chain),
    {Left, Right}.

% % %% @doc dumb iterative version that drown on large input.
% insert(Chain = [ChainHead | ChainTail], Lookup) ->
%     Pairs = to_pairs(Chain),
%     Insertions = [maps:get(Pair, Lookup) || Pair <- Pairs],
%     NewChain = lists:flatten([
%         ChainHead
%         | [[Inserted, Original] || {Inserted, Original} <- lists:zip(Insertions, ChainTail)]
%     ]),
%     {NewChain, Lookup}.

% to_pairs([]) -> [];
% to_pairs([_]) -> [];
% to_pairs([ A, B | Tail]) -> [{A, B} | to_pairs([B | Tail])].

last(List) -> lists:last(List).
first(List) -> hd(List).

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

% A simple counter I reimplement every once in a while :-/
count(List) ->
    count(List, #{}).
count([], Counter) ->
    Counter;
count([H | T], Counter) ->
    Increment = fun(N) -> N + 1 end,
    UpdatedCounter = maps:update_with(H, Increment, _Init = 1, Counter),
    count(T, UpdatedCounter).

