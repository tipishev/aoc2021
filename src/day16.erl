-module(day16).

%% API exports
-export([part1/1, part2/1]).

%% debug
-export([decode_hex/1, print/1]).

% VVV + TTT + 0AAAA
-define(MIN_PACKET_SIZE, 11).

%%% types

-type packet() :: map().
-type bits() :: bitstring().

part1(Filename) ->
    parse(Filename).

part2(Filename) ->
    parse(Filename).

packet_type(4) -> literal;
packet_type(_AnythingBut4) -> operator.

decode_hex(Hex) ->
    decode(binary:decode_hex(Hex)).

%% decode/1
-spec decode(PacketData :: bits()) -> {Packet :: packet(), Tail :: bits()}.
decode(<<Version:3, Type:3, Payload/bits>>) ->
    PacketType = packet_type(Type),
    {Decoded, Tail} = decode(PacketType, Payload),
    Packet = #{version => Version, packet_type => PacketType, decoded => Decoded},
    {Packet, Tail}.

%% decode/2
-spec decode(PacketType :: operator | literal, Payload :: bits()) ->
    {[packet()] | integer(), Tail :: bits()}.
decode(operator, <<0:1, Length:15, SubpacketsPayload:Length/bits, Tail/bits>>) ->
    Packets = decode_length(Length, SubpacketsPayload),
    {Packets, Tail};
decode(operator, <<1:1, Count:11, Payload/bits>>) ->
    {Packets, Tail} = decode_count(Count, Payload),
    {Packets, Tail};
decode(literal, Payload) ->
    {Decoded, Tail} = decode(continuous, Payload, _Acc = <<>>),
    Literal = binary:decode_unsigned(pad_to_bytes(Decoded)),
    {Literal, Tail}.

% decode/3
decode(continuous, <<0:1, Payload:4, Tail/bits>>, Acc) ->
    {<<Acc/bitstring, Payload:4>>, Tail};
decode(continuous, <<1:1, Payload:4, Continuation/bits>>, Acc) ->
    decode(continuous, Continuation, <<Acc/bitstring, Payload:4>>).

-spec decode_length(Length :: non_neg_integer(), Payload :: bits()) -> Packets :: [packet()].
decode_length(Length, Payload) ->
    decode_length(Length, Payload, _Packets = []).


-spec decode_count(Count :: non_neg_integer(), Payload :: bits()) -> Packets :: [packet()].
decode_count(Count, Payload) ->
    decode_count(Count, Payload, _Packets=[]).

decode_length(RemainingLength, _Payload, Packets) when RemainingLength < ?MIN_PACKET_SIZE ->
    Packets;
decode_length(_RemainingLength, Payload, Packets) ->
    {Packet, Tail} = decode(Payload),
    decode_length(bit_size(Tail), Tail, [Packet | Packets]).

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
