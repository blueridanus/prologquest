:- module(command, [
    read_command/2,
    write_effect/2
]).

:- use_module(library(http/json)).
:- use_module(util, [
    read_u32/2,
    write_u32/2
]).

read_command(ReadStream, Command) :-
    read_frame_size(ReadStream, _), % fixme: frame size is being ignored
    json_read(ReadStream, Json),
    parse_command_json(Json, Command).

parse_command_json(Json, Command) :-
    command_declaration(Json, simple), !,
    Command=Json.

parse_command_json(Json, Command) :-
    Json=json([Tag=Data]),
    command_declaration(Tag, compound(string)), !,
    Command =.. [Tag, Data].

parse_command_json(Json, _) :- 
    throw(command_parse_error(Json)).

command_declaration(query, compound(string)).
command_declaration(extend, compound(string)).
command_declaration(enumerate_more, simple).
command_declaration(read_response, compound(string)).
command_declaration(halt, simple).

write_effect(WriteStream, Effect) :-
    effect_declaration(Effect, simple), !,
    atom_string(Effect, Name),
    atom_json_term(Serialized, Name, []),
    write_serialized_effect(WriteStream, Serialized).

write_effect(WriteStream, Effect) :-
    Effect =.. [Name, Data],
    effect_declaration(Name, compound(string)), !,
    format("Inside write_effect, effect name: ~w, effect: ~w~n", [Name, Effect]),
    ( string(Data)
    ->  String=Data
    ;   term_string(Data, String)
    ),
    format("Term string: ~w~n", [String]),
    atom_json_term(Serialized, json([Name=String]), [as(string)]),
    write_serialized_effect(WriteStream, Serialized).

write_serialized_effect(WriteStream, Serialized) :-
    format("Writing serialized effect, JSON: ~w~n", [Serialized]),
    new_memory_file(Mem),
    open_memory_file(Mem, write, MemWriteStream, [encoding(utf8)]),
    write(MemWriteStream, Serialized),
    close(MemWriteStream),
    size_memory_file(Mem, Size, octet),
    set_stream(WriteStream, encoding(octet)),
    write_frame_size(WriteStream, Size),
    set_stream(WriteStream, encoding(utf8)),
    write(WriteStream, Serialized),
    flush_output(WriteStream).

effect_declaration(ok, simple).
effect_declaration(answer, compound(string)).
effect_declaration(more, compound(string)).
effect_declaration(halted, simple).
effect_declaration(error, compound(string)).
effect_declaration(write, compound(string)).
effect_declaration(read, simple).

read_frame_size(ReadStream, Size) :-
    read_u32(ReadStream, Size).

write_frame_size(WriteStream, Size) :-
    write_u32(WriteStream, Size).

:- begin_tests(commands_effects).

test(unicode) :- 
    new_memory_file(Mem),
    open_memory_file(Mem, write, MemWriteStream, [encoding(octet)]),
    write_effect(MemWriteStream, answer("X = emoji(\"🤌\")")).

:- end_tests(commands_effects).