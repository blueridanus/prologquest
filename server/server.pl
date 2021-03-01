:- use_module(library(socket)).
:- use_module(library(sandbox)).
:- use_module('discord_emoji', [discord_emoji/2, discord_emoji/1, debug_emoji/1]).

custom_safe(assertz(X)) :- custom_safe_assert(X).
custom_safe(asserta(X)) :- custom_safe_assert(X).
custom_safe(assert(X)) :- custom_safe_assert(X).
custom_safe(discord_emoji(_, _)).
custom_safe(debug_emoji(_)).

custom_safe_assert(_Head:-_Body).

create_server(Port) :-
    tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 10),
    tcp_open_socket(Socket, AcceptFd, _),
    dispatch(AcceptFd).

dispatch(AcceptFd) :-
    tcp_accept(AcceptFd, Socket, Peer),
    thread_create(process_client(Socket, Peer), _,
                    [ detached(true)
                    ]),
    dispatch(AcceptFd).

process_client(Socket, Peer) :-
    setup_call_cleanup(
        tcp_open_socket(Socket, StreamPair),
        handle_service(StreamPair),
        close(StreamPair)).

handle_service(StreamPair) :- 
    stream_pair(StreamPair, ReadStream, WriteStream),
    try_parse_from_stream(ReadStream, ParseResult),
    handle_parsed(WriteStream, ParseResult).

handle_parsed(WriteStream, ParseResult) :- 
    (   
        ParseResult = ok(Query),
        try_query_and_write_result(WriteStream, Query),
        flush_output(WriteStream)
    );
    (
        ParseResult = error(_, JustError),
        message_to_string(error("", JustError), ErrorMessage),
        write(WriteStream, ErrorMessage),
        flush_output(WriteStream)
    ).

try_query_and_write_result(WriteStream, Query) :- 
    (
        ground(Query),
        try_extend(Query, Result),
        write_result(WriteStream, Result, base)
    );
    (
        try_extend(Query, Result),
        write_result(WriteStream, Result, non_base)
    ).


write_result(WriteStream, exception(Err), _) :- 
    write(WriteStream, "Query error: "),
    message_to_string(Err, ErrorMessage),
    write(WriteStream, ErrorMessage).

write_result(WriteStream, the(_), base) :- 
    write(WriteStream,"true.").

write_result(WriteStream, the(Output), non_base) :- 
    write(WriteStream,Output).

write_result(WriteStream, no, _) :- 
    write(WriteStream, "false.").

check_safety(Goal) :- 
    custom_safe(Goal);
    safe_goal(Goal).

try_extend(Query, Result) :- 
    catch((
        check_safety(Query),
        engine_create(Query,Query,Engine),
        engine_next_reified(Engine, Output), 
        Result = Output
    ), Err, Result = exception(Err)).
    
    

try_parse_from_stream(ReadStream, Result) :-
    catch(parse_from_stream(ReadStream, Result), Err, Result = Err).

parse_from_stream(ReadStream, Result) :-
    Result = ok(Output),
    read(ReadStream, Output).

:- initialization(create_server(3966)).