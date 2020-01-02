:- module(sat_solver,[solvername/1,to_cnf/2, solve/1]).
% Export the predicates for the test module later on.
%:- module(sat_solver,[solvername/1, to_cnf/2, solve/1]).

:- load_test_files([]).

% solvername\1
solvername(irgendein_name).

% to_cnf/2

simplify(not(Term1), not(Term2)):-
    !,
    simplify(Term1, Term2).

% a => b  (<=>) -a v b
simplify(implies(Term1, Term2), or(not(SimpTerm1),SimpTerm2)):-
    !,
    simplify(Term1, SimpTerm1),
    simplify(Term2, SimpTerm2).

simplify(and(Term1, Term2), and(SimpTerm1, SimpTerm2)):-
    !,
    simplify(Term1, SimpTerm1),
    simplify(Term2, SimpTerm2).

simplify(or(Term1, Term2), or(SimpTerm1, SimpTerm2)):-
    !,
    simplify(Term1, SimpTerm1),
    simplify(Term2, SimpTerm2).

simplify(X,X).

move(not(not(Term1)), MovTerm1):-
    !,
    move(Term1, MovTerm1).

% De Morgan's laws
move(not(or(Term1,Term2)), and(MovTerm1,MovTerm2)):-
    !,
    move(not(Term1), MovTerm1),
    move(not(Term2), MovTerm2).

move(not(and(Term1,Term2)), or(MovTerm1,MovTerm2)):-
    !,
    move(not(Term1), MovTerm1),
    move(not(Term2), MovTerm2).

% Distributive property
move(or(Term1, and(Term2,Term3)), and(or(NewTerm1,NewTerm2), or(NewTerm1,NewTerm3))):-
    !,
    move(Term1, NewTerm1),
    move(Term2, NewTerm2),
    move(Term3, NewTerm3).

move(or(and(Term1,Term2),Term3), and(or(NewTerm1, NewTerm3), or(NewTerm2, NewTerm3))):-
    !,
    move(Term1, NewTerm1),
    move(Term2, NewTerm2),
    move(Term3, NewTerm3).

move(and(Term1, Term2), and(SimpTerm1, SimpTerm2)):-
    !,
    move(Term1, SimpTerm1),
    move(Term2, SimpTerm2).

move(or(Term1, Term2), or(SimpTerm1, SimpTerm2)):-
    !,
    move(Term1, SimpTerm1),
    move(Term2, SimpTerm2).

move(X,X).



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
    move(SimpTerm, MoveTerm),
%    distri(MoveTerm, DistriTerm),
    to_list(MoveTerm, Result).

% solve/1

solve([]):-!.
%solve([[]]):-!,fail.

solve([Head|Tail]):-
    member(true, Head),!,
    solve(Tail).

solve([Head|Tail]):-
    member(not(false), Head),
    solve(Tail).
