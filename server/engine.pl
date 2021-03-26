:- module(engine, [
    execute_command/3,
    make_context/2
]).

:- use_module(command, [write_effect/2]).
:- use_module(answer, [execute_engine_goal/3]).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(assertz(C)) :- safe_rule(C).
sandbox:safe_primitive(asserta(C)) :- safe_rule(C).
sandbox:safe_primitive(retract(C)) :- safe_rule(C).

safe_rule((M:Head :- Body)) :- !, M = user, safe_body(Body).
safe_rule((Head :- Body)) :- safe_body(Body).

safe_body(Body) :- safe_goal(Body).

% execute_command(Context, Command, ProcessedContext).
execute_command(Context, query(QueryString), ProcessedContext) :-
    try_parse_query(QueryString, ParseResult, VarNames),
    post_effect(Context.effect_stream, Context.effect_mutex, ok),
    answer_query(Context, ParseResult, VarNames, ProcessedContext).

execute_command(Context, extend(RulesString), ProcessedContext) :- fail. % unimplemented

execute_command(Context, enumerate_more, ProcessedContext) :-
    Context = ProcessedContext,
    thread_send_message(Context.current_engine_thread, next_answer).

execute_command(Context, read_response(ReadString), ProcessedContext) :- fail. % unimplemented

execute_command(Context, halt, ProcessedContext) :-
    Context.current_engine_thread \= none
    ->
        thread_signal(Context.current_engine_thread, abort),
        mutex_unlock(Context.effect_mutex).

% created by service handler
make_context(Context, EffectStream) :-
    mutex_create(MutexId),
    thread_self(ThreadId),
    Context = context{
        effect_mutex: MutexId,
        effect_stream: EffectStream,
        command_processing_thread: ThreadId,
        current_engine_thread: none,
        current_engine: none
    }.

%start_engine(Context, Goal, StartedContext) :-

post_effect(EffectStream, EffectMutex, Effect) :- 
    mutex_lock(EffectMutex),
    write_effect(EffectStream, Effect),
    mutex_unlock(EffectMutex).

try_parse_query(QueryString, Result, VarNames) :-
    catch(parse_query(QueryString, Result, VarNames), Err, Result = bad(Err)).

parse_query(QueryString, Result, VarNames) :-
    Result = ok(Output),
    read_term_from_atom(QueryString, Output, [variable_names(VarNames)]).

answer_query(Context, ok(Query), VarNames, ProcessedContext) :-
    catch((
            safe_goal(Query),
            answer_safe_query(Context, Query, VarNames, ProcessedContext)
        ),
        SafetyError,
        answer_query(Context, bad(SafetyError), _, _)
    ).

answer_query(Context, bad(ParseError), _, _) :-
    message_to_string(ParseError, ErrorString),
    post_effect(Context.effect_stream, Context.effect_mutex, error(ErrorString)),
    stop_command_loop(Context.effect_stream, Context.effect_mutex).

:- dynamic engine_worker_thread/2.
:- volatile engine_worker_thread/2.

:- dynamic get_effect_output/2.
:- volatile get_effect_output/2.

user:message_hook(_, silent, _) :- !, fail.

user:message_hook(Term, query, Lines) :-
    Term = query(Type),
    functor(Type, more, _),
    engine_self(EngineId), !,
    get_effect_output(EngineId, EffectStream, EffectMutex),
    print_message_lines(string(String), "", Lines),
    post_effect(EffectStream, EffectMutex, more(String)).

user:message_hook(Term, query, Lines) :-
    Term = query(Type),
    engine_self(EngineId), !,
    get_effect_output(EngineId, EffectStream, EffectMutex),
    halt_worker_thread(EngineId, EffectMutex),
    print_message_lines(string(String), "", Lines),
    post_effect(EffectStream, EffectMutex, answer(String)).

user:message_hook(Term, error, Lines) :-
    engine_self(EngineId), 
    get_effect_output(EngineId, EffectStream, EffectMutex), !,
    halt_worker_thread(EngineId, EffectMutex),
    print_message_lines(string(String), "", Lines),
    post_effect(EffectStream, EffectMutex, error(String)).

% FIXME: writing effects is not atomic, thread may be halted while writing to stream! 
% message effects back from spawned thread instead, so those can always be properly written
answer_safe_query(Context, Query, Bindings, ProcessedContext) :-
    EffectStream = Context.effect_stream,
    EffectMutex = Context.effect_mutex,
    engine_create(_, ( 
        catch(
            execute_engine_goal(Query, Bindings, _),
            Err,
            print_message(error, Err)
        )
    ), QueryEngine, [stack(60000000)]),
    thread_create(
        init_query_thread(QueryEngine, Context),
        EngineThread,
        [stack(70000000)]
    ),
    assertz(get_effect_output(QueryEngine, EffectStream, EffectMutex)), % fixme: cleanup
    assertz(engine_worker_thread(QueryEngine, EngineThread)), % fixme: cleanup
    thread_send_message(EngineThread, next_answer),
    ProcessedContext = Context
        .put(current_engine_thread, EngineThread)
        .put(current_engine, QueryEngine).

halt_worker_thread(EngineId, EffectMutex) :-
    engine_worker_thread(EngineId, ThreadId),
    thread_send_message(ThreadId, done).

init_query_thread(QueryEngine, Context) :-
    EffectStream = Context.effect_stream,
    EffectMutex = Context.effect_mutex,
    CommandThread = Context.command_processing_thread,
    thread_at_exit(stop_command_loop(CommandThread, EffectMutex)),
    query_thread(QueryEngine, EffectStream, EffectMutex).

stop_command_loop(CommandThread, EffectMutex) :-
    thread_signal(CommandThread, (
        mutex_lock(EffectMutex),
        abort    
    )).

query_thread(Engine, EffectStream, EffectMutex) :-
    thread_get_message(Message),
    Message = next_answer, % Message = done -> fail
    engine_next_reified(Engine, _),
    query_thread(Engine, EffectStream, EffectMutex).

:- begin_tests(engine).

once_query_test(Query, WrittenEffects) :-
    new_memory_file(Mem), 
    open_memory_file(Mem, write, WS, [encoding(octet)]),
    thread_create((
            make_context(Context, WS),
            try_parse_query(Query, ParseResult, VarNames),
            answer_query(Context, ParseResult, VarNames, ProcessedContext),
            thread_get_message(_)
        ), Id),
    thread_join(Id, _),
    close(WS),
    memory_file_to_string(Mem, WrittenEffects, utf8).

more_query_test(Query, WrittenEffects) :-
    new_memory_file(Mem), 
    open_memory_file(Mem, write, WS, [encoding(octet)]),
    thread_create((
        make_context(Context, WS),
        try_parse_query(Query, ParseResult, VarNames),
        answer_query(Context, ParseResult, VarNames, ProcessedContext),
        thread_send_message(ProcessedContext.current_engine_thread, done),
        thread_join(ProcessedContext.current_engine_thread, _)
    ), Id),
    thread_join(Id, _),
    close(WS),
    memory_file_to_string(Mem, WrittenEffects, utf8).

test(simple_true) :-
    once_query_test("true.", String),
    ExpectedString = "\000\\000\\000\\026\{\"answer\":\"true.\\n\\n\"}".
    %assertion(ExpectedString = String).

test(simple_false) :-
    once_query_test("false.", String),
    ExpectedString = "\000\\000\\000\\027\{\"answer\":\"false.\\n\\n\"}",
    assertion(ExpectedString = String).

test(once) :-
    once_query_test("X is 2+1, Y is 5+3.", String),
    ExpectedString = "\000\\000\\000\\037\{\"answer\":\"X = 3,\\nY = 8.\\n\\n\"}",
    assertion(ExpectedString = String).

test(more_simple) :-
    more_query_test("member(X, [1,2,3]).", String),
    ExpectedString = "\000\\000\\000\\021\{\"more\":\"X = 1 \"}",
    assertion(ExpectedString = String).

test(exception_clpfd) :-
    use_module(library(clpfd)),
    once_query_test("X #= 0/0.", String),
    ExpectedString = "\000\\000\\000\\021\{\"more\":\"X = 1 \"}",
    assertion(ExpectedString = String).

test(exception_unknown) :-
    once_query_test("throw(bad).", String),
    ExpectedString = "\000\\000\\000\\"{\"error\":\"Unknown message: bad\\n\"}",
    assertion(ExpectedString = String).

/*
test(exception_emoji) :-
    once_query_test("X#>emoji(\"🤌\"), X#<emoji(\"🤏\").", String),
    ExpectedString = "...",
    assertion(ExpectedString = String).
*/
:- end_tests(engine).