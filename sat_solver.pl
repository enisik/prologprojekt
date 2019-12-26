:- module(sat_solver,[solvername/1,to_cnf/2, solve/1]).
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

% --a (<=>) a
to_cnf(not(not(X)), Res):-
    !,
    to_cnf(X, Res).

% a => b  (<=>) -a v b
to_cnf(implies(Term1,Term2),Result):-
    to_cnf(or(not(Term1),Term2),Result).

% De-Morgansche Regel
to_cnf(not(or(Term1,Term2)),Result):-
    !,
    to_cnf(and(not(Term1),not(Term2)),Result).

to_cnf(not(and(Term1,Term2)), Result):-
    !,
    to_cnf(or(not(Term1),not(Term2)), Result).

% Distributivgesetz
to_cnf(or(Term1, and(Term2,Term3)),[Res1,Res2]):-
    !,
    to_cnf(or(Term1,Term2),[Res1]),
    to_cnf(or(Term1,Term3),[Res2]).

% and
to_cnf(and(Term1, Term2), [Res1, Res2]):-
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, [Res2]).

% or
to_cnf(or(lit(X), lit(Y)), [[X,Y]]):-!.

to_cnf(or(not(lit(X)), lit(Y)), [[not(X),Y]]):-!.
to_cnf(or(lit(X), not(lit(Y))), [[X,not(Y)]]):-!.

to_cnf(or(not(not(X)),Y),Result):-
    !,
    to_cnf(or(X,Y),Result).

to_cnf(or(X),not(not(Y)),Result):-
    !,
    to_cnf(or(X,Y),Result).

%Kommutativität
%to_cnf(or(X,Y), [Res2, Res1]):-
%    !,
%    to_cnf(or(Y,X), [Res1, Res2]).

% TODO solve

solve([]).
solve([[]]):-fail.
