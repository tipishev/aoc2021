-module(day2).

%% API exports
-export([part1/1, part2/1]).

part1(File) ->
    Commands = read_commands(File),
    {X, Y} = get_position(Commands),
    X * Y.

part2(File) ->
    Commands = read_commands(File),
    State0 = #{x => 0, y => 0, aim => 0},
    #{x := X, y := Y} = lists:foldl(fun update/2, State0, Commands),
    X * Y.

get_position(Commands) ->
    Deltas = [get_position_delta(Command) || Command <- Commands],
    DeltasSum = lists:foldl(
        fun({Dx, Dy}, {X, Y}) -> {X + Dx, Y + Dy} end,
        _InitialPosition = {0, 0},
        Deltas
    ),
    DeltasSum.

get_position_delta({forward, X}) -> {X, 0};
get_position_delta({up, Y}) -> {0, -Y};
get_position_delta({down, Y}) -> {0, Y}.


update({down, D}, State = #{aim := Aim0}) -> State#{aim := Aim0 + D};
update({up, D}, State = #{aim := Aim0}) -> State#{aim := Aim0 - D};
update({forward, D}, State = #{x := X0, y := Y0, aim := Aim}) -> State#{x := X0 + D, y := Y0 + Aim * D}.

%%====================================================================
%% Santa's little helpers
%%====================================================================

read_commands(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = string:lexemes(FileContent, "\n"),
    [
        begin
            [DirectionBin, MagnitudeBin] = string:lexemes(Line, " "),
            Direction = binary_to_existing_atom(DirectionBin),
            Magnitude = binary_to_integer(MagnitudeBin),
            {Direction, Magnitude}
        end
        || Line <- Lines
    ].
