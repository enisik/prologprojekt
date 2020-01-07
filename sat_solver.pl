:- module(sat_solver,[solvername/1,to_cnf/2, solve/1]).
% Export the predicates for the test module later on.
%:- module(sat_solver,[solvername/1, to_cnf/2, solve/1]).

:- load_test_files([]).

% solvername\1
solvername(prosat).

% to_cnf/2

% a => b  (<=>) -a v b
simplify(implies(Term1, Term2), or(not(SimpTerm1),SimpTerm2)):-
    !,
    simplify(Term1, SimpTerm1),
    simplify(Term2, SimpTerm2).



% De Morgan's laws
simplify(not(or(Term1,Term2)), and(SimpTerm1,SimpTerm2)):-
    !,
    simplify(not(Term1), SimpTerm1),
    simplify(not(Term2), SimpTerm2).

simplify(not(and(Term1,Term2)), or(SimpTerm1,SimpTerm2)):-
    !,
    simplify(not(Term1), SimpTerm1),
    simplify(not(Term2), SimpTerm2).

simplify(not(not(Term1)), SimpTerm1):-
    !,
    simplify(Term1, SimpTerm1).

% Distributive property
simplify(or(Term1, and(Term2,Term3)), and(or(SimpTerm1,SimpTerm2), or(SimpTerm1,SimpTerm3))):-
    !,
    simplify(Term1, SimpTerm1),
    simplify(Term2, SimpTerm2),
    simplify(Term3, SimpTerm3).

simplify(or(and(Term1,Term2),Term3), and(or(SimpTerm1, SimpTerm3), or(SimpTerm2, SimpTerm3))):-
    !,
    simplify(Term1, SimpTerm1),
    simplify(Term2, SimpTerm2),
    simplify(Term3, SimpTerm3).

simplify(and(Term1, Term2), and(SimpTerm1, SimpTerm2)):-
    !,
    simplify(Term1, SimpTerm1),
    simplify(Term2, SimpTerm2).

simplify(or(Term1, Term2), or(SimpTerm1, SimpTerm2)):-
    !,
    simplify(Term1, SimpTerm1),
    simplify(Term2, SimpTerm2).

%simplify(not(Term1), not(SimpTerm1)):-
%    !,
%    simplify(Term1, SimpTerm1).

simplify(X,X).

to_list(and(Term1, Term2), [Res1, Res2]):-
    !,
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, [Res2]).

to_list(or(Term1, Term2), [Result]):-
    !,
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, [Res2]),
    append(Res1, Res2, Result).

to_cnf(lit(X), [[X]]):-!.
to_cnf(not(lit(X)), [[not(X)]]):-
    \+ ground(X), !.
to_cnf(not(lit(true)), [[false]]):-!.
to_cnf(not(lit(false)),[[true]]):-!.

to_cnf(Term, Result):-
    !,
    simplify(Term, SimpTerm),
    simplify(SimpTerm, SimpTerm2),
    to_list(SimpTerm2, Result).

% solve/1

solve([]):-!.
%solve([[]]):-!,fail.

solve([Head|Tail]):-
    member(true, Head),!,
    solve(Tail).

solve([Head|Tail]):-
    member(not(false), Head),
    solve(Tail).
