:- module(command, [
    read_command/2,
    write_effect/2
]).

:- use_module(utf8_pl).

% -- Serialization of commands from stream

read_command(ReadStream, Command) :- 
    read_u64(ReadStream, _), % ignore frame size
    read_u32(ReadStream, Code),
    (
        command_declaration(Command, Code, [string]),
            read_serialized_string(ReadStream, String),
            Command =.. [Name, String]
    ;   command_declaration(Command, Code, []) 
    ).

command_declaration(query(_), 0, [string]).
command_declaration(extend(_), 1, [string]).
command_declaration(enumerate_more, 2, []).
command_declaration(read_response(_), 3, [string]).
command_declaration(halt, 4, []).

/*
read_command_with_code(ReadStream, Code, Command) :-
    \+ command_declaration(Command, Code, _),
    throw(command_error("Unknown command type")).
*/
% -- Serialization of effects to stream

write_effect(WriteStream, Effect) :- 
    is_effect(Effect), !,
    write_effect_size(WriteStream, Effect),
    write_effect_code(WriteStream, Effect),
    write_effect_data(WriteStream, Effect).

write_effect(WriteStream, Effect) :-
    \+ is_effect(Effect),
    throw(effect_error("Unknown effect type")).

effect_declaration(ok, 0, simple).
effect_declaration(answer(_), 1, compound_kind(string)).
effect_declaration(more(_), 2, compound_kind(string)).
effect_declaration(halted, 3, simple).
effect_declaration(error(_), 4, compound_kind(string)).
effect_declaration(write(_), 5, compound_kind(string)).
effect_declaration(read, 6, simple).

is_effect(Effect) :- effect_declaration(Effect, _, _).

write_effect_size(WriteStream, Effect) :-
    effect_declaration(Effect, _, simple),
    write_u64(WriteStream, 4).

write_effect_size(WriteStream, Effect) :-
    effect_declaration(Effect, _, compound_kind(string)),
    Effect =.. [_, String],
    utf8_length(String, Length),
    Size = 4 + Length,
    write_u64(WriteStream, Size).

write_effect_code(WriteStream, Effect) :-
    effect_declaration(Effect, Code, _),
    write_u32(WriteStream, Code).

write_effect_data(WriteStream, Effect) :-
    effect_declaration(Effect, _, simple), !.

write_effect_data(WriteStream, Effect) :-
    effect_declaration(Effect, _, compound_kind(string)),
    Effect =.. [_, String],
    write_serialized_string(WriteStream, String).

% FIXME: what if size read =/= size expected?
read_serialized_string(ReadStream, String) :-
    read_u64(ReadStream, ExpectedSize),
    read_delimited_string(ReadStream, ExpectedSize, String).

read_delimited_string(ReadStream, ExpectedSize, String) :-
    get_bytes(ReadStream, ExpectedSize, Bytes),
    phrase(utf8_codes(Bytes), String).

read_u32(ReadStream, Number) :- 
    get_bytes(ReadStream, 4, Bytes),
    bytes_u32(Bytes, Number).

write_serialized_string(WriteStream, String) :-
    utf8_length(String, Length), 
    write_u64(WriteStream, Length),
    write(WriteStream, String).

write_u32(WriteStream, Number) :-
    bytes_u32(Bytes, Number),
    put_bytes(WriteStream, Bytes).

bytes_u32(Bytes, Number) :-
    ground(Bytes),
    bytes_to_u32(Bytes, Number),
    !.

bytes_u32(Bytes, Number) :-
    ground(Number),
    u32_to_bytes(Number, Bytes),
    !.

bytes_u32(_, _) :-
    throw(instantiation_error("Either bytes B0 to B3 or Number")). 
    % probably unintended usage ^ of instantiation_error(Term) above

% little endian
bytes_to_u32(Bytes, Number) :-
    Bytes = [B0, B1, B2, B3],
    Number is B0 + B1 << 8 + B2 << 16 + B3 << 24.

% little endian
u32_to_bytes(Number, Bytes) :-
    Bytes = [B0, B1, B2, B3],
    B0 is Number /\ 255,
    B1 is (Number >> 8) /\ 255,
    B2 is (Number >> 16) /\ 255,
    B3 is (Number >> 24) /\ 255.

read_u64(ReadStream, Number) :- 
    get_bytes(ReadStream, 8, Bytes),
    bytes_u64(Bytes, Number).

write_u64(WriteStream, Number) :-
    bytes_u64(Bytes, Number),
    put_bytes(WriteStream, Bytes).

get_bytes_dcg(ReadStream, N) --> 
    {
        N > 0,
        get_byte(ReadStream, Byte),
        M is N-1
    },
    [Byte], get_bytes_dcg(ReadStream, M).

get_bytes_dcg(_, 0) --> [].

get_bytes(ReadStream, N, Bytes) :-
    phrase(get_bytes_dcg(ReadStream, N), Bytes).

get_bytes(ReadStream, Bytes) :-
    length(Bytes, N),
    phrase(get_bytes_dcg(ReadStream, N), Bytes).

put_bytes(_, []).
put_bytes(WriteStream, [Byte | T]) :-
    put_byte(WriteStream, Byte),
    put_bytes(WriteStream, T).

bytes_u64(Bytes, Number) :-
    ground(Bytes),
    bytes_to_u64(Bytes, Number),
    !.

bytes_u64(Bytes, Number) :-
    ground(Number),
    u64_to_bytes(Number, Bytes),
    !.

bytes_u64(_, _) :-
    throw(instantiation_error("Either bytes B0 to B7 or Number")). 
    % probably unintended usage ^ of instantiation_error(Term) above

% little endian
bytes_to_u64(Bytes, Number) :-
    Bytes = [B0, B1, B2, B3, B4, B5, B6, B7],
    Number is 
        B0 + B1 << 8 + B2 << 16 + B3 << 24 + B4 << 32 + B5 << 40 + B6 << 48 + B7 << 56.

% little endian
u64_to_bytes(Number, Bytes) :-
    Bytes = [B0, B1, B2, B3, B4, B5, B6, B7],
    B0 is Number /\ 255,
    B1 is (Number >> 8) /\ 255,
    B2 is (Number >> 16) /\ 255,
    B3 is (Number >> 24) /\ 255,
    B4 is (Number >> 32) /\ 255,
    B5 is (Number >> 40) /\ 255,
    B6 is (Number >> 48) /\ 255,
    B7 is (Number >> 56) /\ 255.

utf8_length(String, Length) :-
    new_memory_file(Mem),
    insert_memory_file(Mem, 0, String),
    memory_file_to_codes(Mem, Codes, utf8),
    length(Codes, Length).

:- begin_tests(primitive_serialization).

test(utf8_length_empty) :-
    String = "",
    utf8_length(String, Length),
    assertion(Length=0).

test(utf8_length_basic) :-
    String = "Hello World!",
    utf8_length(String, Length),
    assertion(Length=12).

test(utf8_length_cjk) :-
    String = "中国",
    utf8_length(String, Length),
    assertion(Length=6).

test(utf8_length_cuneiform_single) :-
    String = "𒀆",
    utf8_length(String, Length),
    assertion(Length=4).

test(utf8_length_cuneiform_multiple) :-
    String = "𒀲𒀻𒀽",
    utf8_length(String, Length),
    assertion(Length=12).

test(utf8_length_zalgo) :-
    String = "p͆̏͂r͗͆͒o̓̾̎lͦ͋̈ŏͮͩg͆ͪ̔҉̧͝ͅ",
    utf8_length(String, Length),
    assertion(Length=50).

test(utf8_length_emoji_combiners) :-
    String = "🧑🏼‍🦰",
    utf8_length(String, Length),
    assertion(Length=15).

:- end_tests(primitive_serialization).

:- begin_tests(effect_serialization).

test(write_effect_basic_simple) :-
    new_memory_file(Handle),
    open_memory_file(Handle, write, WStream, [encoding(octet)]),
    write_effect(WStream, halted),
    close(WStream),
    open_memory_file(Handle, read, RStream, [encoding(octet)]),
    get_bytes(RStream, 8, LengthBytes),
    assertion(LengthBytes=[4,0,0,0,0,0,0,0]),
    get_bytes(RStream, 4, EffectTypeBytes),
    assertion(EffectTypeBytes=[3,0,0,0]),
    close(RStream),
    size_memory_file(Handle, FileSize),
    free_memory_file(Handle),
    % 8 bytes for u64 length + 4 bytes for u32 discriminator 
    assertion(FileSize=12). 

test(write_effect_basic_compound) :-
    new_memory_file(Handle),
    open_memory_file(Handle, write, WStream, [encoding(octet)]),
    write_effect(WStream, more("Hello")),
    close(WStream),
    open_memory_file(Handle, read, RStream, [encoding(octet)]),
    get_bytes(RStream, 8, LengthBytes),
    assertion(LengthBytes=[9,0,0,0,0,0,0,0]),
    get_bytes(RStream, 4, EffectTypeBytes),
    assertion(EffectTypeBytes=[2,0,0,0]),
    close(RStream),
    size_memory_file(Handle, FileSize),
    free_memory_file(Handle),
    % 8 bytes for u64 length + 4 bytes for u32 discriminator + 8 bytes for u64 string length + 5 utf8 bytes
    assertion(FileSize=25). 

test(read_command_basic_simple) :-
    new_memory_file(Handle),
    open_memory_file(Handle, write, WStream, [encoding(octet)]),
    put_bytes(WStream, [4,0,0,0,0,0,0,0,2,0,0,0]),
    close(WStream),
    open_memory_file(Handle, read, RStream, [encoding(octet)]),
    read_command(RStream, Command),
    close(RStream),
    assertion(Command=enumerate_more),
    free_memory_file(Handle).

test(read_command_basic_simple_fail) :-
    new_memory_file(Handle),
    open_memory_file(Handle, write, WStream, [encoding(octet)]),
    put_bytes(WStream, [4,0,0,0,0,0,0,0,2,0,0,0]),
    close(WStream),
    open_memory_file(Handle, read, RStream, [encoding(octet)]),
    read_command(RStream, Command),
    close(RStream),
    assertion(Command\=halt),
    free_memory_file(Handle).

test(read_command_basic_compound) :-
    new_memory_file(Handle),
    open_memory_file(Handle, write, WStream, [encoding(octet)]),
    % fixme: frame length wrong
    put_bytes(WStream, [4,0,0,0,0,0,0,0,3,0,0,0,6,0,0,0,0,0,0,0]),
    write(WStream, "foobar"),
    close(WStream),
    open_memory_file(Handle, read, RStream, [encoding(octet)]),
    read_command(RStream, Command),
    close(RStream),
    assertion(Command=read_response("foobar")),
    free_memory_file(Handle).

test(read_command_basic_compound_fail, [fail]) :-
    new_memory_file(Handle),
    open_memory_file(Handle, write, WStream, [encoding(octet)]),
    % fixme: frame length wrong
    put_bytes(WStream, [4,0,0,0,0,0,0,0,3,0,0,0,9,0,0,0,0,0,0,0]),
    write(WStream, "quxquxqux"),
    close(WStream),
    open_memory_file(Handle, read, RStream, [encoding(octet)]),
    read_command(RStream, Command),
    Command = extend(_),
    format("~w~n",Command).


:- end_tests(effect_serialization).


/* 
new_memory_file(Handle),
open_memory_file(Handle, write, WStream, [encoding(octet)]),
% fixme: frame length wrong
command:put_bytes(WStream, [4,0,0,0,0,0,0,0,3,0,0,0,6,0,0,0,0,0,0,0]),
write(WStream, "cacaca"),
close(WStream),
open_memory_file(Handle, read, RStream, [encoding(octet)]),
read_command(RStream, Command),
format("~w~n",Command),
close(RStream),
free_memory_file(Handle). 
*/