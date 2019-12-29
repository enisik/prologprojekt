:- module(sat_solver,[solvername/1,to_cnf/2, solve/1]).
% Export the predicates for the test module later on.
%:- module(sat_solver,[solvername/1, to_cnf/2, solve/1]).

:- load_test_files([]).

% solvername\1
solvername(irgendein_name).

% to_cnf/2

to_cnf(lit(X), [[X]]).
to_cnf(not(lit(X)), [[not(X)]]):-
    \+ ground(X), !.
to_cnf(not(lit(true)), [[false]]).
to_cnf(not(lit(false)),[[true]]).

% --a (<=>) a
to_cnf(not(not(X)), Res):-
    !,
    to_cnf(X, Res).

% a => b  (<=>) -a v b
to_cnf(implies(Term1,Term2),Result):-
    !,
    to_cnf(or(not(Term1),Term2),Result).

% -(a = > b) (<=>) -(-a v b) (<=>) a and -b
to_cnf(not(implies(Term1, Term2)), Result):-
    !,
    to_cnf(and(Term1, not(Term2)), Result).

% De Morgan's laws
to_cnf(not(or(Term1,Term2)),Result):-
    !,
    to_cnf(and(not(Term1),not(Term2)),Result).

to_cnf(not(and(Term1,Term2)), Result):-
    !,
    to_cnf(or(not(Term1),not(Term2)), Result).

% Distributive property
to_cnf(or(Term1, and(Term2,Term3)),[Res1,Res2]):-
    !,
    to_cnf(or(Term1,Term2),[Res1]),
    to_cnf(or(Term1,Term3),[Res2]).

to_cnf(or(and(Term1,Term2),Term3), [Res1,Res2]):-
    !,
    to_cnf(or(Term1, Term3), [Res1]),
    to_cnf(or(Term2, Term3), [Res2]).

% and
to_cnf(and(Term1, Term2), [Res1, Res2]):-
    to_cnf(Term1, [Res1]),
    to_cnf(Term2, [Res2]).

% or
to_cnf(or(lit(X), lit(Y)), [[X,Y]]):-!.

to_cnf(or(not(lit(X)), lit(Y)), [[not(X),Y]]):-!.
to_cnf(or(lit(X), not(lit(Y))), [[X,not(Y)]]):-!.

to_cnf(or(not(implies(Term1, Term2)), Term3), Result):-
    !,
    to_cnf(or(and(Term1, not(Term2)), Term3), Result).

to_cnf(or(Term1, not(implies(Term2, Term3))), Result):-
    !,
    to_cnf(or(Term1, and(Term2, not(Term3))), Result).

to_cnf(or(not(not(Term1)),Term2),Result):-
    !,
    to_cnf(or(Term1,Term2),Result).

to_cnf(or(Term1),not(not(Term2)),Result):-
    !,
    to_cnf(or(Term1,Term2),Result).

to_cnf(or(Term1, not(or(Term2, Term3))), Result):-
    to_cnf(or(Term1, and(not(Term2), not(Term3))), Result).

to_cnf(or(and(Term1, Term2), Term3), Result):-
    to_cnf(and(Term1, or(Term2, Term3)), Result).

to_cnf(or(or(Term1, Term2), Term3), [[Term1, Term2, Term3]]).

% commutative property
%to_cnf(or(X,Y), [Res2, Res1]):-
%    !,
%    to_cnf(or(Y,X), [Res1, Res2]).

% TODO solve

solve([]):-!.
solve([[]]):-!,fail.

solve([Head|Tail]):-
    member(true, Head),!,
    %remove_true([Tail], Result)
    solve(Tail).

solve([Head|Tail]):-
    member(not(false), Head),
    solve(Tail).

%remove_true([Head|Tail], [Head|TResult]):-
%    member(true, Head),
%    remove_true(Tail, TResult).

%remove_true([Head|Tail], [Head|TResult]):-
%    member(not(false), Head),
%    remove_true(Tail, TResult).
%remove_true([_|Tail], TResult):-
%    remove_true(Tail, TResult).
