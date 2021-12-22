-module(day16).

%% API exports
-export([part1/1, part2/1]).

%% debug
-export([decode_hex/1, print/1, sum_versions/1, calculate/1, product/1]).

% VVV + TTT + 0AAAA
-define(MIN_PACKET_SIZE, 11).

%%% types

-type packet() :: map().
-type bits() :: bitstring().
-type packet_type() :: sum | product | minimum | maximum | literal | greater_than | less_than | equal_to.

part1(Filename) ->
    sum_versions(parse(Filename)).

sum_versions(#{packet_type := literal, version := Version}, Acc) ->
    Acc + Version;
sum_versions(
    #{packet_type := _Operator, version := Version, decoded := Packets},
    Acc
) ->
    Acc + Version + lists:sum([sum_versions(Packet, 0) || Packet <- Packets]).

sum_versions(Hex) ->
    sum_versions(decode_hex(Hex), _Acc = 0).

part2(Filename) ->
    calculate(parse(Filename)).

calculate(Hex) ->
    do_calculate(decode_hex(Hex)).

do_calculate(#{packet_type := literal, decoded := Literal}) ->
    Literal;
do_calculate(#{packet_type := sum, decoded := Packets}) ->
    lists:sum([do_calculate(Packet) || Packet <- Packets]);
do_calculate(#{packet_type := product, decoded := Packets}) ->
    product([do_calculate(Packet) || Packet <- Packets]);
do_calculate(#{packet_type := minimum, decoded := Packets}) ->
    lists:min([do_calculate(Packet) || Packet <- Packets]);
do_calculate(#{packet_type := maximum, decoded := Packets}) ->
    lists:max([do_calculate(Packet) || Packet <- Packets]);
do_calculate(#{packet_type := less_than, decoded := [PacketA, PacketB]}) ->
    case do_calculate(PacketA) > do_calculate(PacketB) of
        true -> 1;
        false -> 0
    end;
do_calculate(#{packet_type := greater_than, decoded := [PacketA, PacketB]}) ->
    case do_calculate(PacketA) < do_calculate(PacketB) of
        true -> 1;
        false -> 0
    end;
do_calculate(#{packet_type := equal_to, decoded := [PacketA, PacketB]}) ->
    case do_calculate(PacketA) =:= do_calculate(PacketB) of
        true -> 1;
        false -> 0
    end.

-spec packet_type(Type :: 0..7) -> packet_type().
packet_type(0) -> sum;
packet_type(1) -> product;
packet_type(2) -> minimum;
packet_type(3) -> maximum;
packet_type(4) -> literal;
packet_type(5) -> greater_than;
packet_type(6) -> less_than;
packet_type(7) -> equal_to.

decode_hex(Hex) ->
    {Decoded, _Tail} = decode(binary:decode_hex(Hex)),
    Decoded.

%% decode/1
-spec decode(PacketData :: bits()) -> {Packet :: packet(), Tail :: bits()}.
decode(<<Version:3, Type:3, Payload/bits>>) ->
    PacketType = packet_type(Type),
    {Decoded, Tail} = decode(PacketType, Payload),
    Packet = #{version => Version, packet_type => PacketType, decoded => Decoded},
    {Packet, Tail}.

%% decode/2
-spec decode(PacketType :: packet_type(), Payload :: bits()) -> {[packet()] | integer(), Tail :: bits()}.
decode(literal, Payload) ->
    {Decoded, Tail} = decode_literal(Payload, _Acc = <<>>),
    Literal = binary:decode_unsigned(pad_to_bytes(Decoded)),
    {Literal, Tail};
decode(_Operator, <<0:1, Length:15, SubpacketsPayload:Length/bits, Tail/bits>>) ->
    Packets = decode_length(Length, SubpacketsPayload),
    {Packets, Tail};
decode(_Operator, <<1:1, Count:11, Payload/bits>>) ->
    {Packets, Tail} = decode_count(Count, Payload),
    {Packets, Tail}.

%%% Integer literal with continuation bit

decode_literal(<<0:1, Payload:4, Tail/bits>>, Acc) ->
    {<<Acc/bitstring, Payload:4>>, Tail};
decode_literal(<<1:1, Payload:4, Continuation/bits>>, Acc) ->
    decode_literal(Continuation, <<Acc/bitstring, Payload:4>>).

%%% Length-based list

-spec decode_length(Length :: non_neg_integer(), Payload :: bits()) -> Packets :: [packet()].
decode_length(Length, Payload) ->
    decode_length(Length, Payload, _Packets = []).

decode_length(RemainingLength, _Payload, Packets) when RemainingLength < ?MIN_PACKET_SIZE ->
    Packets;
decode_length(_RemainingLength, Payload, Packets) ->
    {Packet, Tail} = decode(Payload),
    decode_length(bit_size(Tail), Tail, [Packet | Packets]).

%%% Count-based list

-spec decode_count(Count :: non_neg_integer(), Payload :: bits()) -> Packets :: [packet()].
decode_count(Count, Payload) ->
    decode_count(Count, Payload, _Packets = []).

decode_count(Count, Payload, Packets) when length(Packets) =:= Count ->
    {Packets, Payload};
decode_count(Count, Payload, Packets) ->
    {Packet, Tail} = decode(Payload),
    decode_count(Count, Tail, [Packet | Packets]).

% %% @doc turn a bit string into bytes by prepending enough leading zeros.
pad_to_bytes(Bitstring) when is_bitstring(Bitstring) ->
    PadLength = 8 - bit_size(Bitstring) rem 8,
    <<0:PadLength, Bitstring/bits>>.

print(Binary) -> lists:flatten([integer_to_list(Digit) || <<Digit:1>> <= Binary]).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [HexString] = string:lexemes(FileContent, "\n"),
    HexString.

product([]) -> 1;
product([H | T]) -> H * product(T).
