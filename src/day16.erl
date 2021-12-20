-module(day16).

%% API exports
-export([part1/1, part2/1]).

%% debug
-export([decode/1, print/1]).

part1(Filename) ->
    parse(Filename).

part2(Filename) ->
    parse(Filename).

packet_type(4) -> literal;
packet_type(_AnythingBut4) -> operator.

%% decode/1
decode(<<Version:3, Type:3, Payload/bits>>) ->
    PacketType = packet_type(Type),
    Decoded = decode(PacketType, Payload),
    Decoded#{version => Version, packet_type => PacketType}.

%% decode/2
decode(operator, <<0:1, SubpacketsLength: 15, Payload/bits>>) ->
    decode(subpackets_length, SubpacketsLength, Payload);
decode(operator, <<1:1, SubpacketsCount: 11, Payload/bits>>) ->
    decode(subpackets_count, SubpacketsCount, Payload);
decode(literal, Payload) ->
    #{decoded := Decoded, tail := Tail} = decode(continuous, Payload, _Acc = <<>>),
    Literal = binary:decode_unsigned(pad_to_bytes(Decoded)),
    Consumed = bit_size(Payload) - bit_size(Tail),
    #{decoded => Literal, tail => Tail, consumed => Consumed}.

% decode/3
decode(subpackets_length, SubpacketsLength, Payload) ->
    decode(subpackets_length, SubpacketsLength, Payload, _PacketAcc = []);
decode(subpackets_count, SubpacketsCount, Payload) ->
    {subpackets_count, SubpacketsCount, Payload};
decode(continuous, <<0:1, Payload:4, Tail/bits>>, Acc) ->
    #{decoded => <<Acc/bitstring, Payload:4>>, tail => Tail};
decode(continuous, <<1:1, Payload:4, Continuation/bits>>, Acc) ->
    decode(continuous, Continuation, <<Acc/bitstring, Payload:4>>).

% decode/4

decode(subpackets_length, SubpacketsLength, _Payload, PacketAcc) when SubpacketsLength =< 0 ->
    PacketAcc;
decode(subpackets_length, SubpacketsLength, Payload, PacketAcc) ->
    Packet = #{tail := Tail, consumed := Consumed} = decode(Payload),
    decode(subpackets_length, SubpacketsLength - Consumed, Tail, [Packet | PacketAcc]).

% %% @doc turn a bit string into bytes by prepending enough leading zeros.
pad_to_bytes(Bitstring) when is_bitstring(Bitstring) ->
    PadLength = 8 - bit_size(Bitstring) rem 8,
    <<0:PadLength, Bitstring/bits>>.

print(Binary) -> lists:flatten([integer_to_list(Digit) || <<Digit:1>> <= Binary]).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [HexString] = string:lexemes(FileContent, "\n"),
    HexString.
