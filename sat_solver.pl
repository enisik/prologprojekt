:- module(sat_solver,[solvername/1,to_cnf/2, solve/1]).
% Export the predicates for the test module later on.
%:- module(sat_solver,[solvername/1, to_cnf/2, solve/1]).

:- load_test_files([]).

% solvername\1
solvername(proSAT).

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


distri_loop(Term1, Result):-
    simplify_distri(Term1, Term2),
    !,
    distri_loop(Term2, Result).
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

remove_false([],[]):-!.
remove_false([Head|Tail], Result):-
    atom(Head),!,
    Head == false,
    remove_false(Tail,Result).
remove_false([Head|Tail], [Head|TResult]):-
    remove_false(Tail,TResult).

remove_value([],[]):-!.
remove_value([Head|Tail], Result):-
    member(X, Head),
    ground(X),
    member(X, [true]),!,
    remove_value(Tail,Result).
remove_value([Head|Tail], [NewHead|TResult]):-
    remove_false(Head,NewHead),!,
    remove_value(Tail,TResult).
remove_value(List, List).

unit_propagate([],[]):-!.
unit_propagate([[Var]|Tail], Tail):-
    var(Var),
    Var = true.
unit_propagate([[not(Var)|Tail]], Tail):-
    var(Var),
    Var = false.
unit_propagate([Head|Tail], [Head|TResult]):-
    unit_propagate(Tail,TResult).

unit_prop_and_remove(List,Result):-
    remove_value(List, ClearList),
    unit_propagate(ClearList, NewList),
    remove_value(NewList,Result).

propagate([Head|Tail], Result):-
    member(X, Head),
    var(X),
    member(X, [true,false]),
    remove_value([Head|Tail], Result).


solve_helper(Term,N):-
    N >0,!,
    NN is N -1,
    unit_prop_and_remove(Term, NewTerm),
    solve_helper(NewTerm, NN).

solve_helper(Term, N):-
    N >0,!,
    NN is N-1,
    propagate(Term, NewTerm),
    solve_helper(NewTerm,NN).

solve_helper([], _):-!.
solve_helper(Term, _):- member([], Term), !, fail.

%solve([]):-!.
%solve(Term):- member([], Term), !, fail.
solve(Term):-
    unit_prop_and_remove(Term, Term1),!,
    solve_helper(Term1, 10).
