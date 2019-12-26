:- module(sat_solver,[solvername/1,to_cnf/2]).
% Export the predicates for the test module later on.
%:- module(sat_solver,[solvername/1, to_cnf/2, solve/1]).

:- load_test_files([]).

% solvername\1
solvername(irgendein_name).

% to_cnf/2
to_cnf(lit(X), [[X]]).
to_cnf(not(not(X)), Res):-
    !,
    to_cnf(X, Res).
to_cnf(not(lit(true)), [[false]]):-!.
to_cnf(not(lit(false)),[[true]]):-!.
to_cnf(not(lit(X)), [[not(X)]]).

to_cnf(not(or(X,Y)),Res):-
    !,
    to_cnf(and(not(X),not(Y)),Res).

to_cnf(and(Term1, Term2), [Res1, Res2]):-
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, [Res2]).


% TODO

to_cnf(or(lit(X), lit(Y)), [[X,Y]]).
to_cnf(or(not(lit(X)), lit(Y)), [[not(X),Y]]).
to_cnf(or(X,Y), Res):-
    to_cnf(or(Y,X), Res).
