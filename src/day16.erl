-module(day16).

%% API exports
-export([part1/1, part2/1]).

%% debug
-export([decode/1, print/1]).

part1(Filename) ->
    parse(Filename).

part2(Filename) ->
    parse(Filename).

decode(<<Version:3, Type:3, Payload/bits>>) ->
    PacketType = packet_type(Type),
    DecodedPacket = decode(PacketType, Payload),
    #{version => Version, packet_type => PacketType, payload => DecodedPacket}.

decode(operator, <<LengthTypeId:1, SubpacketsLength: 15, _Payload/bits>>)
 when LengthTypeId =:= 0 ->
    {subpackets_length, SubpacketsLength};
decode(operator, <<LengthTypeId:1, _NumberOfSubpackets: 11, Payload/bits>>)
  when LengthTypeId =:= 1 ->
    % {number_of_subpackets, NumberOfSubpackets};
    decode(Payload);
decode(literal, Payload) ->
    binary:decode_unsigned(pad_to_bytes(decode_continuous(Payload, _Acc = <<>>))).

decode_continuous(<<ContinuationBit:1, Payload:4, Rest/bits>>, Acc0) ->
    Acc = <<Acc0/bitstring, Payload:4>>,
    case ContinuationBit of
        0 -> Acc;
        1 -> decode_continuous(Rest, Acc)
    end.

pad_to_bytes(Bitstring) when is_bitstring(Bitstring) ->
    BitSize = bit_size(Bitstring),
    PadLength = 8 - BitSize rem 8,
    <<0:PadLength, Bitstring/bits>>.


packet_type(2) -> operator;
packet_type(3) -> operator;
packet_type(4) -> literal;
packet_type(6) -> operator.

print(Binary) -> lists:flatten([integer_to_list(Digit) || <<Digit:1>> <= Binary]).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [HexString] = string:lexemes(FileContent, "\n"),
    HexString.
