:- module(sat_solver,[solvername/1,to_cnf/2, solve/1]).
% Export the predicates for the test module later on.
%:- module(sat_solver,[solvername/1, to_cnf/2, solve/1]).

:- load_test_files([]).

% solvername\1
solvername(prosat).

%%____ to_cnf/2 ____%%

% simplify_implies ------------------------------------------------------------

% a => b  (<=>) -a v b
simplify_implies(implies(Term1, Term2), or(not(SimpTerm1),SimpTerm2)):-
    !,
    simplify_implies(Term1, SimpTerm1),
    simplify_implies(Term2, SimpTerm2).

% simplify inner expression
simplify_implies(not(Term1), not(SimpTerm1)):-
    !,
    simplify_implies(Term1, SimpTerm1).

simplify_implies(and(Term1, Term2),and(SimpTerm1, SimpTerm2)):-
    !,
    simplify_implies(Term1, SimpTerm1),
    simplify_implies(Term2, SimpTerm2).

simplify_implies(or(Term1, Term2),or(SimpTerm1, SimpTerm2)):-
    !,
    simplify_implies(Term1, SimpTerm1),
    simplify_implies(Term2, SimpTerm2).

simplify_implies(Term,Term).
%-----------------------------------------------------------------------------

% simplify_demorgan and not(not(X)) = X --------------------------------------

% De Morgan's laws
simplify_demorgan(not(or(Term1,Term2)), and(SimpTerm1,SimpTerm2)):-
    !,
    simplify_demorgan(not(Term1), SimpTerm1),
    simplify_demorgan(not(Term2), SimpTerm2).

simplify_demorgan(not(and(Term1,Term2)), or(SimpTerm1,SimpTerm2)):-
    !,
    simplify_demorgan(not(Term1), SimpTerm1),
    simplify_demorgan(not(Term2), SimpTerm2).

% not(not(X)) = X
simplify_demorgan(not(not(Term1)), SimpTerm1):-
    !,
    simplify_demorgan(Term1, SimpTerm1).

%simplify inner expression
simplify_demorgan(not(Term1), not(SimpTerm1)):-
    !,
    simplify_demorgan(Term1, SimpTerm1).

simplify_demorgan(and(Term1, Term2),and(SimpTerm1, SimpTerm2)):-
    !,
    simplify_demorgan(Term1, SimpTerm1),
    simplify_demorgan(Term2, SimpTerm2).

simplify_demorgan(or(Term1, Term2),or(SimpTerm1, SimpTerm2)):-
    !,
    simplify_demorgan(Term1, SimpTerm1),
    simplify_demorgan(Term2, SimpTerm2).

simplify_demorgan(Term,Term).
%------------------------------------------------------------------------------

%simplify_distributive property ---------------------------------------------

% Distributive property
simplify_distri(or(Term1, and(Term2,Term3)), and(or(Term1,Term2), or(Term1,Term3))):- !.

simplify_distri(or(and(Term1,Term2),Term3), and(or(Term1, Term3), or(Term2, Term3))):-!.


simplify_distri(and(Term1, Term2), and(SimpTerm1, Term2)):-
    simplify_distri(Term1, SimpTerm1).

simplify_distri(and(Term1, Term2), and(Term1, SimpTerm2)):-
    simplify_distri(Term2, SimpTerm2).

simplify_distri(or(Term1, Term2), or(SimpTerm1, Term2)):-
    simplify_distri(Term1, SimpTerm1).

simplify_distri(or(Term1, Term2), or(Term1, SimpTerm2)):-
    simplify_distri(Term2, SimpTerm2).

%simplify_distri(Term,Term).

distri_loop(Term1, Term2):-
    simplify_distri(Term1, Acc),
    !,
    distri_loop(Acc, Term2).
distri_loop(Term, Term).

% to_list converts cnf to list
to_list(lit(X), [[X]]):-!.
to_list(not(lit(X)), [[not(X)]]):-
    var(X), !.
to_list(not(lit(true)), [[false]]):-!.
to_list(not(lit(false)),[[true]]):-!.

to_list(and(Term1, Term2), Result):-
    !,
    to_list(Term1, Res1),
    to_list(Term2, Res2),
    append(Res1,Res2, Result).

to_list(or(Term1, Term2), [Result]):-
    !,
    to_list(Term1, [Res1]),
    to_list(Term2, [Res2]),
    append(Res1, Res2, Result).

to_cnf(Term, Result):-
    !,
    simplify_implies(Term, SimpImpTerm),
    simplify_demorgan(SimpImpTerm, SimpDeMorganTerm),
    distri_loop(SimpDeMorganTerm,SimpTerm),
    to_list(SimpTerm, Result).

% solve/1

remove_value([],[]):-!.
remove_value([Head|Tail], Result):-
    member(X, Head),
    ground(X),
    member(X, [true]),
    !,
    remove_value(Tail,Result).
remove_value([Head|Tail], [NewHead|TResult]):-
    member(X, Head),
    atom(X),
    delete(Head, X, NewHead),!,
    remove_value(Tail,TResult).
remove_value(List, List).

unit_porpagate([],[]).
unit_porpagate([[Var]|Tail], [[Var]|TResult]):-
    var(Var),!,
    Var = true,
    unit_porpagate(Tail, TResult).
unit_porpagate([Head|Tail], [Head|TResult]):-
    unit_porpagate(Tail,TResult).

solve([], 1):-!.
solve(Term, 1):- member([], Term), !, fail.

solve([]):-!.
solve(Term):- member([], Term), !, fail.
solve(Term):-
    unit_porpagate(Term, Term1),
    remove_value(Term1, Term2),!,
    solve(Term2, 1).
