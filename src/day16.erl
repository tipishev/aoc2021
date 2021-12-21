-module(day16).

%% API exports
-export([part1/1, part2/1]).

%% debug
-export([decode_hex/1, print/1]).

part1(Filename) ->
    parse(Filename).

part2(Filename) ->
    parse(Filename).

packet_type(4) -> literal;
packet_type(_AnythingBut4) -> operator.

decode_hex(Hex) ->
    decode(binary:decode_hex(Hex)).

%% decode/1
-spec decode(PacketData :: bitstring()) -> {Packet :: map(), Tail :: bitstring()}.
decode(PacketData) ->
    <<Version:3, Type:3, Payload/bits>> = PacketData,
    PacketType = packet_type(Type),
    {Decoded, Tail} = decode(PacketType, Payload),
    Packet = #{version => Version, packet_type => PacketType, decoded => Decoded},
    {Packet, Tail}.

%% decode/2
decode(operator, <<0:1, Length: 15, Subpackets: Length, _Tail/bits>>) ->
    decode(length, Length, Subpackets);
decode(operator, <<1:1, Count: 11, Payload/bits>>) ->
    decode(count, Count, Payload);
decode(literal, Payload) ->
    #{decoded := Decoded, tail := Tail} = decode(continuous, Payload, _Acc = <<>>),
    Literal = binary:decode_unsigned(pad_to_bytes(Decoded)),
    Consumed = bit_size(Payload) - bit_size(Tail),
    #{decoded => Literal, consumed => Consumed, tail => Tail}.

% decode/3
decode(length, Length, Payload) ->
    decode(length, Length, Payload, _PacketAcc = []);
decode(count, SubpacketsCount, Payload) ->
    {not_implemented, subpackets_count, SubpacketsCount, Payload};

decode(continuous, <<0:1, Payload:4, Tail/bits>>, Acc) ->
    #{decoded => <<Acc/bitstring, Payload:4>>, tail => Tail};
decode(continuous, <<1:1, Payload:4, Continuation/bits>>, Acc) ->
    decode(continuous, Continuation, <<Acc/bitstring, Payload:4>>).

% decode/4
decode(length, RemainingLength, _Payload, Packets) when RemainingLength < 4 ->
    Packets;
decode(length, RemainingLength, Payload, Packets) ->
    {Packet = #{length := PacketLength}, Tail} = decode(Payload),
    decode(length, RemainingLength - PacketLength, Tail, [Packet | Packets]).

% %% @doc turn a bit string into bytes by prepending enough leading zeros.
pad_to_bytes(Bitstring) when is_bitstring(Bitstring) ->
    PadLength = 8 - bit_size(Bitstring) rem 8,
    <<0:PadLength, Bitstring/bits>>.

print(Binary) -> lists:flatten([integer_to_list(Digit) || <<Digit:1>> <= Binary]).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [HexString] = string:lexemes(FileContent, "\n"),
    HexString.
