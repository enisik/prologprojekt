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

move(and(Term1, Term2), and(SimpTerm1, SimpTerm2)):-
    !,
    move(Term1, SimpTerm1),
    move(Term2, SimpTerm2).

move(or(Term1, Term2), or(SimpTerm1, SimpTerm2)):-
    !,
    move(Term1, SimpTerm1),
    move(Term2, SimpTerm2).

move(X,X).


% Distributive property
distri(or(Term1, and(Term2,Term3)), and(or(Term1,Term2), or(Term1,Term3))):-!.

distri(or(and(Term1,Term2),Term3), and(or(Term1, Term3), or(Term2, Term3))):-!.

distri(X,X).

to_list(and(Term1, Term2), [Res1, Res2]):-
    !,
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, [Res2]).

to_list(or(Term1, Term2), [[Res1, Res2]]):-
    !,
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, [Res2]).


to_cnf(lit(X), [[X]]):-!.
to_cnf(not(lit(X)), [[not(X)]]):-
    \+ ground(X), !.
to_cnf(not(lit(true)), [[false]]):-!.
to_cnf(not(lit(false)),[[true]]):-!.

%simp_list([],[]):-!.
simp_list([[[H1]],[[H2]]|Tail], [H1|TResult]):-
    !,
    simp_list([[H2]|Tail], TResult).

simp_list(X,X).

to_cnf(Term, Result):-
    !,
    simplify(Term, SimpTerm),
    move(SimpTerm, MoveTerm),
    distri(MoveTerm, DistriTerm),
    to_list(DistriTerm, ListResult),
    list_simp(ListResult, Result).

% solve/1

solve([]):-!.
%solve([[]]):-!,fail.

%solve([Head|Tail]):-
%    member(true, Head),!,
    %remove_true([Tail], Result)
%    solve(Tail).

%solve([Head|Tail]):-
%    member(not(false), Head),
%    solve(Tail).
