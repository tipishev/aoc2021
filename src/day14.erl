-module(day14).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    {Template, Lookup} = parse(Filename),
    polymerize(Template, Lookup, 10).
    % max_min_diff(Polymerized).

part2(Filename) ->
    {Template, Lookup} = parse(Filename),
    _Polymerized = polymerize(Template, Lookup, 23).
    % Values = maps:values(count(Polymerized)),
    % lists:max(Values) - lists:min(Values).

polymerize(Template, _Lookup, _Steps) ->
    _PairFrequencies = count(to_pairs(Template)),
    _CharFrequencies = count(Template).

% step(PairFrequencies, )

% Internal

% The main part

% polimerize(Chain, _Lookup, _StepsLeft = 0) ->
%     Chain;
% polimerize(Chain, Lookup, StepsLeft) ->
%     io:format("StepsLeft: ~p~n", [StepsLeft]),
%     {NewChain, NewLookup} = insert(Chain, Lookup),
%     polimerize(NewChain, NewLookup, StepsLeft - 1).


% Graveyard

% insert([], Lookup) -> {[], Lookup};
% insert([A], Lookup) -> {[A], Lookup};
% insert([A, B], Lookup) -> {[A, maps:get([A, B], Lookup), B], Lookup};
% insert(Chain, Lookup) ->
%     case maps:is_key(Chain, Lookup) of
%         true ->
%             {maps:get(Chain, Lookup), Lookup};
%         false ->
%             {Body, Butt} = chop_the_butt(Chain),
%             Middle = maps:get([last(Body), Butt], Lookup),
%             {NewLeft, NewLeftLookup} = insert(Body, Lookup),
%             NewChain = NewLeft ++ [Middle] ++ [Butt],
%             NewLookup = NewLeftLookup#{Chain => NewChain},
%             {NewChain, NewLookup}
%     end.

% last(List) -> lists:last(List).

% chop_the_butt(List) ->
%     [Butt | ReverseBody] = lists:reverse(List),
%     Body = lists:reverse(ReverseBody),
%     {Body, Butt}.

% % %% @doc slightly faster solution with halving and reassembling a lookup, drowns at ~25
% insert([], Lookup) -> {[], Lookup};
% insert([A], Lookup) -> {[A], Lookup};
% insert([A, B], Lookup) -> {[A, maps:get([A, B], Lookup), B], Lookup};
% insert(Chain, Lookup) ->
%     case maps:is_key(Chain, Lookup) of
%         true ->
%             {maps:get(Chain, Lookup), Lookup};
%         false ->
%             {Left, Right} = split_in_half(Chain),
%             Middle = maps:get([last(Left), first(Right)], Lookup),
%             {NewLeft, NewLeftLookup} = insert(Left, Lookup),
%             {NewRight, NewRightLookup} = insert(Right, Lookup),
%             NewChain = NewLeft ++ [Middle] ++ NewRight,
%             MergedLookup = maps:merge(NewLeftLookup, NewRightLookup),
%             NewLookup = MergedLookup#{Chain => NewChain},
%             {NewChain, NewLookup}
%     end.

% last(List) -> lists:last(List).
% first(List) -> hd(List).


% split_in_half(List) ->
%     MiddleIdx = length(List) div 2,
%     Left = lists:sublist(List, MiddleIdx),
%     Right = lists:nthtail(MiddleIdx, List),
%     {Left, Right}.

% % %% @doc dumb iterative version that drowns on large input.
% insert(Chain = [ChainHead | ChainTail], Lookup) ->
%     Pairs = to_pairs(Chain),
%     Insertions = [maps:get(Pair, Lookup) || Pair <- Pairs],
%     NewChain = lists:flatten([
%         ChainHead
%         | [[Inserted, Original] || {Inserted, Original} <- lists:zip(Insertions, ChainTail)]
%     ]),
%     {NewChain, Lookup}.

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
to_pairs([ A, B | Tail]) -> [{A, B} | to_pairs([B | Tail])].

% A simple counter I reimplement every once in a while :-/
count(List) ->
    count(List, #{}).
count([], Counter) ->
    Counter;
count([H | T], Counter) ->
    Increment = fun(N) -> N + 1 end,
    UpdatedCounter = maps:update_with(H, Increment, _Init = 1, Counter),
    count(T, UpdatedCounter).
