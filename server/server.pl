:- use_module(library(socket)).
:- use_module(library(sandbox)).

:- use_module(engine, [
    execute_command/3,
    make_context/2
]).

:- use_module(command, [
    read_command/2    
]).

:- use_module('discord_emoji', [
    discord_emoji/2, 
    discord_emoji/1, 
    debug_emoji/1]).

:- use_module(library(clpfd)).

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
    stream_pair(StreamPair, ReadStream, WriteStream),
    make_context(Context, WriteStream),
    commands_loop(ReadStream, Context).
    

commands_loop(ReadStream, Context) :-
    read_command(ReadStream, Command),
    execute_command(Context, Command, ProcessedContext),
    commands_loop(ReadStream, ProcessedContext).