-module(day16).

%% API exports
-export([part1/1, part2/1]).

%% debug
-export([decode_hex/1, print/1]).

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
    { [packet()] | integer(), Tail :: bits()}.
decode(operator, <<0:1, Length: 15, SubpacketsPayload:Length/bits, Tail/bits>>) ->
    io:format("Length: ~p, SubpacketsPayload: ~p~n", [Length, SubpacketsPayload]),
    Packets = decode_length(Length, SubpacketsPayload),
    {Packets, Tail};
decode(operator, <<1:1, Count: 11, Payload/bits>>) ->
    decode(count, Count, Payload);
decode(literal, Payload) ->
    {Decoded, Tail} = decode(continuous, Payload, _Acc = <<>>),
    Literal = binary:decode_unsigned(pad_to_bytes(Decoded)),
    {Literal, Tail}.

% decode/3
decode(count, SubpacketsCount, Payload) ->
    {not_implemented, subpackets_count, SubpacketsCount, Payload};
decode(continuous, <<0:1, Payload:4, Tail/bits>>, Acc) ->
    {<<Acc/bitstring, Payload:4>>, Tail};
decode(continuous, <<1:1, Payload:4, Continuation/bits>>, Acc) ->
    decode(continuous, Continuation, <<Acc/bitstring, Payload:4>>).

% the trash bits at the end should not be discarded
-spec decode_length(Length :: non_neg_integer(), Payload :: bits()) -> Packets :: [packet()].
decode_length(Length, Payload) ->
    decode_length(Length, Payload, _Packets = []).

% decode/4

% FIXME stop condition is most likely incorrect
decode_length(RemainingLength, _Payload, Packets) when RemainingLength < 4 ->
    Packets;
decode_length(RemainingLength, Payload, Packets) ->
    {Packet, Tail} = decode(Payload),
    decode_length(bit_size(Tail), Tail, [Packet | Packets]).

% %% @doc turn a bit string into bytes by prepending enough leading zeros.
pad_to_bytes(Bitstring) when is_bitstring(Bitstring) ->
    PadLength = 8 - bit_size(Bitstring) rem 8,
    <<0:PadLength, Bitstring/bits>>.

print(Binary) -> lists:flatten([integer_to_list(Digit) || <<Digit:1>> <= Binary]).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [HexString] = string:lexemes(FileContent, "\n"),
    HexString.
