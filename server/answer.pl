:- module(answer, [
    write_engine_answer/4
]).
%! execute_engine_goal(+Goal, +Bindings, -Truth) is nondet.
%
% toplevel.pl:1070
execute_engine_goal(Query, Bindings, Truth) :-
    '$current_typein_module'(TypeIn), !,
    setup_call_cleanup(
        '$set_source_module'(M0, TypeIn),
        expand_goal(Query, Goal),
        '$set_source_module'(M0)),
    execute_engine_goal2(Goal, Bindings, Truth).

execute_engine_goal2(Goal, Bindings, true) :- 
    '$current_typein_module'(TypeIn),
    '$toplevel':residue_vars(TypeIn:Goal, Vars, TypeIn:Delays),
    deterministic(Det),
    '$toplevel':call_expand_answer(Bindings, NewBindings),
    write_engine_answer(NewBindings, Vars, Delays, Det).

execute_engine_goal2(_, _, false) :- 
    print_message(query, query(no)).

% toplevel:1132
write_engine_answer(Bindings, ResVars, Delays, Det) :-
    '$current_typein_module'(TypeIn),
    '$toplevel':translate_bindings(Bindings, Bindings1, ResVars, TypeIn:Residuals),
    '$toplevel':omit_qualifier(Delays, TypeIn, Delays1),
    write_engine_answer2(Bindings1, Residuals, Delays1, Det).

write_engine_answer2(Bindings, Residuals, Delays, true) :-
    !, 
    print_message(query, query(yes(Bindings, Delays, Residuals))).

write_engine_answer2(Bindings, Residuals, Delays, _Det) :-
    print_message(query, query(more(Bindings, Delays, Residuals))).