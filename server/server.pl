:- use_module(library(socket)).
:- use_module(library(sandbox)).
:- use_module('discord_emoji', [discord_emoji/2, discord_emoji/1, debug_emoji/1]).
:- use_module(library(clpfd)).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(assertz(_)).
sandbox:safe_primitive(asserta(_)).
sandbox:safe_primitive(assert(_)).
sandbox:safe_primitive(retract(_)).

:- initialization(create_server(3966)).

create_server(Port) :-
    tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 10),
    tcp_open_socket(Socket, AcceptFd, _),
    dispatch(AcceptFd).

dispatch(AcceptFd) :-
    tcp_accept(AcceptFd, Socket, Peer),
    thread_create(process_client(Socket, Peer), _,
                    [ detached(true),
                    stack_limit(85000000)
                    ]),
    dispatch(AcceptFd).

process_client(Socket, Peer) :-
    setup_call_cleanup(
        tcp_open_socket(Socket, StreamPair),
        handle_service(StreamPair),
        close(StreamPair)).

handle_service(StreamPair) :- 
    set_stream(StreamPair, encoding(utf8)),
    set_stream(StreamPair, representation_errors(prolog)),
    stream_pair(StreamPair, ReadStream, WriteStream),
    write(Encoding),
    try_parse_from_stream(ReadStream, ParseResult, VarNames),
    handle_parsed(WriteStream, ParseResult, VarNames).

handle_parsed(WriteStream, ParseResult, VarNames) :- 
    (   
        ParseResult = ok(Query),
        try_query_and_write_result(WriteStream, Query, VarNames),
        flush_output(WriteStream)
    );
    (
        ParseResult = error(_, _),
        message_to_string(ParseResult, ErrorMessage),
        write(WriteStream, ErrorMessage),
        flush_output(WriteStream)
    ).

try_query_and_write_result(WriteStream, Query, VarNames) :- 
    try_extend(Query, Result),
    write_result(WriteStream, Result, Query, VarNames).

write_result(WriteStream, exception(Err), _, _) :- 
    write(WriteStream, "Query error: "),
    message_to_string(Err, ErrorMessage),
    write(WriteStream, ErrorMessage).

write_result(WriteStream, no, _, _) :- 
    write(WriteStream, "false.").

write_result(WriteStream, the(Output), Query, VarNames) :- 
    Query =@= Output
    -> write(WriteStream,"true.")
    ; (
        unifiable(Query, Output, Bindings),
        write_all_bindings(WriteStream, Bindings, VarNames)
    ).

write_all_bindings(WriteStream, [], _).
write_all_bindings(WriteStream, [Binding|T], Names) :-
    (T = []
        -> 
        write_binding(WriteStream, Binding, Names, ".\n")
        ;
        write_binding(WriteStream, Binding, Names, ",\n")
    ),
    write_all_bindings(WriteStream, T, Names).


write_binding(WriteStream, Binding, Names, Separator) :-
    is_named(Binding, Names)
    ->
        write_term(WriteStream, Binding, [variable_names(Names)]),
        write(WriteStream, Separator).
    

is_named(Binding, Names) :-
    member(Name, Names),
    split_eq_term(Binding, Variable, _),
    split_eq_term(Name, _, Variable).

split_eq_term(Term, Variable, Value) :-
    term_string(Term, String),
    sub_string(String, Before, _, After, "="),
    !,
    sub_atom(String, 0, Before, _, Variable),
    sub_string(String, _, After, 0, Value).

try_extend(Query, Result) :- 
    catch((
        safe_goal(Query),
        engine_create(Query,Query,Engine, [stack(60000000)]),
        engine_next_reified(Engine, Output), 
        Result = Output
    ), Err, Result = exception(Err)).

try_parse_from_stream(ReadStream, Result, VarNames) :-
    catch(parse_from_stream(ReadStream, Result, VarNames), Err, Result = Err).

parse_from_stream(ReadStream, Result, VarNames) :-
    Result = ok(Output),
    read_term(ReadStream, Output, [variable_names(VarNames)]).
