:- module(util, [
    get_bytes/3,
    put_bytes/2,
    read_u32/2,
    write_u32/2,
    read_u64/2,
    write_u64/2,
    bytes_u32/2,
    bytes_u64/2
]).

read_u32(ReadStream, Number) :- 
    get_bytes(ReadStream, 4, Bytes),
    bytes_u32(Bytes, Number).

write_u32(WriteStream, Number) :-
    bytes_u32(Bytes, Number),
    put_bytes(WriteStream, Bytes).

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
    Bytes = [B3, B2, B1, B0],
    Number is B0 + B1 << 8 + B2 << 16 + B3 << 24.

% little endian
u32_to_bytes(Number, Bytes) :-
    Bytes = [B3, B2, B1, B0],
    B0 is Number /\ 255,
    B1 is (Number >> 8) /\ 255,
    B2 is (Number >> 16) /\ 255,
    B3 is (Number >> 24) /\ 255.

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
    Bytes = [B7, B6, B5, B4, B3, B2, B1, B0],
    Number is 
        B0 + B1 << 8 + B2 << 16 + B3 << 24 + B4 << 32 + B5 << 40 + B6 << 48 + B7 << 56.

% little endian
u64_to_bytes(Number, Bytes) :-
    Bytes = [B7, B6, B5, B4, B3, B2, B1, B0],
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

:- begin_tests(util).

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

:- end_tests(util).
