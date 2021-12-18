-module(day16).

%% API exports
-export([part1/1, part2/1]).

%% debug
-export([decode/1]).

decode(Hex) ->
    Binary = binary:decode_hex(Hex),
    <<Version:3, Type:3, Payload/bitstring>> = Binary,
    DecodedPayload = decode_payload(Payload),
    DecodedType = decode_type(Type),
    #{version => Version, type => DecodedType, payload => DecodedPayload}.

decode_payload(Payload) ->
    decode_payload(Payload, _Acc = <<>>).

decode_payload(<<ContinuationBit:1, Payload:4, Rest/bitstring>>, Acc) ->
    case ContinuationBit of
        0 -> <<Acc/binary, Payload>>;
        1 -> decode_payload(Rest, <<Acc/binary, Payload>>)
    end.

decode_type(4) ->
    literal.

part1(Filename) ->
    parse(Filename).

part2(Filename) ->
    parse(Filename).

parse(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    [HexString] = string:lexemes(FileContent, "\n"),
    HexString.
