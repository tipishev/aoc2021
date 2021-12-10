-module(day10).

%% API exports
-export([part1/1, part2/1]).

part1(Filename) ->
    Lines = parse(Filename),
    Results = [check(Line) || Line <- Lines],
    Scores = [score_illegal(Illegal)
              || {error, {expected, _Expected, got, Illegal}} <- Results],
    lists:sum(Scores).

score_illegal(round) -> 3;
score_illegal(square) -> 57;
score_illegal(curly) -> 1197;
score_illegal(angly) -> 25137.

part2(Filename) ->
    Lines = parse(Filename),
    Results = [check(Line) || Line <- Lines],
    Incomplete = [Stack || {incomplete, Stack} <- Results],
    Scores = [score_incomplete(Symbols) || Symbols <- Incomplete],
    SortedScores = lists:sort(Scores),
    MiddleIndex = round(math:ceil(length(Scores) / 2)),
    lists:nth(MiddleIndex, SortedScores).

score_completion(round) -> 1;
score_completion(square) -> 2;
score_completion(curly) -> 3;
score_completion(angly) -> 4.

score_incomplete(Symbols) ->
    lists:foldl(fun(Symbol, Score) -> Score * 5 + score_completion(Symbol) end, 0, Symbols).

check(Tokens) ->
    check([], Tokens).

check(_Stack = [], _TokensToProcess = []) -> ok;
check(Stack, _TokensToProcess = []) -> {incomplete, Stack};
check(Stack, [Token | Tokens]) ->
    case advance(Stack, Token) of
        {ok, NewStack} -> check(NewStack, Tokens);
        {error, Reason} -> {error, Reason}
    end.

% open is ok at any time, jus push to the stack
advance(Stack, {open, Type}) -> {ok, [Type | Stack]};
% closing must match
advance([Type | Stack], {close, Type}) -> {ok, Stack};
advance([ExpectedType | _Stack], {close, Type}) -> {error, {expected, ExpectedType, got, Type}};
advance([], {close, Type}) -> {error, {empty, got_close, Type}}.

% Parser

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [[tokenize(<<Char>>) || <<Char>> <= Line] || Line <- Lines].

tokenize(<<"(">>) -> {open, round};
tokenize(<<")">>) -> {close, round};
tokenize(<<"[">>) -> {open, square};
tokenize(<<"]">>) -> {close, square};
tokenize(<<"{">>) -> {open, curly};
tokenize(<<"}">>) -> {close, curly};
tokenize(<<"<">>) -> {open, angly};
tokenize(<<">">>) -> {close, angly}.


