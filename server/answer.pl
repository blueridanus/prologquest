:- module(answer, [
    pretty_error/1,
    pretty_result/1
]).

pretty_error(exception(Error)) :- 
    print_message(error, Error).

pretty_result(no) :- 
    write("false").

pretty_result(the(Instantiation)) :-
    write(Instantiation).
