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
simplify_distri(or(Term1, and(Term2,Term3)), and(or(SimpTerm1,SimpTerm2), or(SimpTerm1,SimpTerm3))):-
    !,
    simplify_distri(Term1, SimpTerm1),
    simplify_distri(Term2, SimpTerm2),
    simplify_distri(Term3, SimpTerm3).

simplify_distri(or(and(Term1,Term2),Term3), and(or(SimpTerm1, SimpTerm3), or(SimpTerm2, SimpTerm3))):-
    !,
    simplify_distri(Term1, SimpTerm1),
    simplify_distri(Term2, SimpTerm2),
    simplify_distri(Term3, SimpTerm3).

simplify_distri(and(Term1, Term2), and(SimpTerm1, SimpTerm2)):-
    !,
    simplify_distri(Term1, SimpTerm1),
    simplify_distri(Term2, SimpTerm2).

simplify_distri(or(Term1, Term2), or(SimpTerm1, SimpTerm2)):-
    !,
    simplify_distri(Term1, SimpTerm1),
    simplify_distri(Term2, SimpTerm2).

simplify_distri(Term,Term).

iterate(Term1, Term2):-
    simplify_distri(Term1, Acc),
    !,
    iterate(Acc, Term2).
iterate(Term, Term).

% to_list converts cnf to list
to_list(lit(X), [[X]]):-!.
to_list(not(lit(X)), [[not(X)]]):-
    \+ ground(X), !.
to_list(not(lit(true)), [[false]]):-!.
to_list(not(lit(false)),[[true]]):-!.

to_list(and(Term1, Term2), [Res1|Res2]):-
    !,
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, Res2).

to_list(or(Term1, Term2), [Result]):-
    !,
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, [Res2]),
    append(Res1, Res2, Result).

to_cnf(Term, Result):-
    !,
    simplify_implies(Term, SimpImpTerm),
    simplify_demorgan(SimpImpTerm, SimpDeMorganTerm),
    iterate(SimpDeMorganTerm,Result).
    %to_list(SimpTerm, Result).

% solve/1
solve(_):-fail.
