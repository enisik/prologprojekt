:- module(sat_solver,[solvername/1,to_cnf/2]).
% Export the predicates for the test module later on.
%:- module(sat_solver,[solvername/1, to_cnf/2, solve/1]).

:- load_test_files([]).

% solvername\1
solvername(irgendein_name).

% to_cnf/2
to_cnf(lit(X), [[X]]).
to_cnf(not(lit(true)), [[false]]):-!.
to_cnf(not(lit(false)),[[true]]):-!.
to_cnf(not(lit(X)), [[not(X)]]).

to_cnf(and(Term1, Term2), [Res1, Res2]):-
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, [Res2]).

%TODO
to_cnf(implies(Term1, Term2), [Res]):-
    .

to_cnf(or(Term1, Term2), [neg(Res1), neg(Res2)]):-
    to_cnf(not(Term1), [Res1]),
    to_cnf(not(Term2), [Res2]).

neg([H|T], [ResH|ResT]):-
    neg()
