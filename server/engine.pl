:- module(engine, [
    execute_command/3,
    make_context/2
]).

:- use_module(command, [write_effect/2]).
:- use_module(answer, [pretty_error/1, pretty_result/1]).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(assertz(_)).
sandbox:safe_primitive(asserta(_)).
sandbox:safe_primitive(assert(_)).
sandbox:safe_primitive(retract(_)).

% execute_command(Context, Command, ProcessedContext).
execute_command(Context, query(QueryString), ProcessedContext) :-
    try_parse_query(QueryString, ParseResult, VarNames),
    post_effect(Context.effect_stream, Context.effect_mutex, ok),
    answer_query(Context, ParseResult, VarNames, ProcessedContext).

execute_command(Context, extend(RulesString), ProcessedContext).

execute_command(Context, enumerate_more, ProcessedContext).

execute_command(Context, read_response(ReadString), ProcessedContext).

execute_command(Context, halt, ProcessedContext) :-
    Context.current_engine_thread \= none
    ->
        thread_signal(Context.current_engine_thread, throw(stop)),
        mutex_unlock(Context.effect_mutex).

% created by service handler
make_context(Context, EffectStream) :-
    mutex_create(MutexId),
    Context = context{
        effect_mutex: MutexId,
        effect_stream: EffectStream,
        current_engine_thread: none,
        current_engine: none
    }.

%start_engine(Context, Goal, StartedContext) :-

post_effect(EffectStream, EffectMutex, Effect) :- 
    format("Posting effect: ~w~n", [Effect]),
    format("Stream: ~w, Mutex: ~w~n", [EffectStream, EffectMutex]),
    mutex_lock(EffectMutex),
    format("Writing effect: ~w~n", [Effect]),
    write_effect(EffectStream, Effect),
    format("Effect written: ~w~n", [Effect]),
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
    post_effect(Context.effect_stream, Context.effect_mutex, error(ErrorString)).

% FIXME: writing effects is not atomic, thread may be halted while writing to stream! 
% message effects back from spawned thread instead, so those can always be properly written
answer_safe_query(Context, Query, VarNames, ProcessedContext) :-
    thread_create(
        init_query_thread(Query, VarNames, Context.effect_stream, Context.effect_mutex),
        EngineThread,
        [stack(70000000)]
    ),
    thread_send_message(EngineThread, next_answer),
    ProcessedContext = Context
        .put(current_engine_thread, EngineThread)
        .put(current_engine, QueryEngine).

user:message_hook(Term, Kind, Lines) :-
    Kind \= silent, 
    engine_self(EngineId),
    format("Message hook called: ~w~n", [Term]),
    get_effect_output(EngineId, EffectStream, EffectMutex),
    format("Pair acquired: ~w, ~w~n", [EffectStream, EffectMutex]),
    print_message_lines(string(String), "", Lines),
    post_effect(EffectStream, EffectMutex, answer(String)).

init_query_thread(Query, Bindings, EffectStream, EffectMutex) :-
    %engine_create(Bindings, '$execute_query'(Query, _, _), QueryEngine, [stack(60000000)]),
    engine_create(Bindings, (
        '$execute_query'(Query, Bindings, _)
    ), QueryEngine, [stack(60000000)]),
    assertz(get_effect_output(QueryEngine, EffectStream, EffectMutex)),
    query_thread(QueryEngine, EffectStream, EffectMutex).

query_thread(Engine, EffectStream, EffectMutex) :-
    thread_get_message(Message),
    Message = next_answer,
    engine_next_reified(Engine, Result),
    (
        Result = exception(_) -> 
        pretty_error(Result) % fixme
    ),
    query_thread(Engine, EffectStream, EffectMutex).

:- begin_tests(engine).

test(simple) :-
    new_memory_file(Mem), 
    open_memory_file(Mem, write, WS, [encoding(octet)]),
    make_context(Context, WS),
    try_parse_query("X is 2+1, Y is 5+3.", ParseResult, VarNames),
    answer_query(Context, ParseResult, VarNames, ProcessedContext),
    sleep(5),
    close(WS),
    open_memory_file(Mem, read, RS, [encoding(octet)]),
    current_output(Out),
    copy_stream_data(RS, Out).

:- end_tests(engine).