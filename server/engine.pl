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
    mutex_lock(EffectMutex),
    write_effect(EffectStream, Effect),
    mutex_unlock(EffectMutex).

try_parse_query(QueryString, Result, VarNames) :-
    catch(parse_query(QueryString, Result, VarNames), Err, Result = bad(Err)).

parse_query(QueryString, Result, VarNames) :-
    Result = ok(Output),
    read_term(QueryString, Output, [variable_names(VarNames)]).

answer_query(Context, ok(Query), VarNames, ProcessedContext) :-
    catch(safe_goal(Query), SafetyError, answer_query(Context, bad(SafetyError), _, _)).

% FIXME: writing effects is not atomic, thread may be halted while writing to stream! 
% message effects back from spawned thread instead, so those can always be properly written
answer_safe_query(Context, Query, VarNames, ProcessedContext) :-
    engine_create(Query,Query,QueryEngine,[stack(60000000)]),
    thread_create(
        query_thread(QueryEngine, Context.effect_stream, Context.effect_mutex),
        EngineThread,
        [stack(70000000)]
    ),
    ProcessedContext = Context
        .put(current_engine_thread, EngineThread)
        .put(current_engine, QueryEngine).

query_thread(Engine, EffectStream, EffectMutex) :-
    thread_get_message(Message),
    Message = answer,
    engine_next_reified(Engine, Result),
    (
        Result = the(Answer),
        with_output_to(string(String), pretty_result(Answer)),
        post_effect(EffectStream, EffectMutex, answer(String))
    ;
        Result = no,
        with_output_to(string(String), pretty_result(no)),
        post_effect(EffectStream, EffectMutex, answer(String))
    ;
        Result = exception(Err),
        with_output_to(string(String), pretty_error(Err)),
        post_effect(EffectStream, EffectMutex, error(String))
    ),
    query_thread(Engine, EffectStream, EffectMutex).

answer_query(Context, bad(ParseError), _, _) :-
    post_effect(Context.effect_stream, Context.effect_mutex, error(ParseError)).