
:- use_module(sat_solver).
:- use_module(library(plunit)).

:- begin_tests(cnf).

test(to_cnf1,[true(Res == [[X],[true]])]) :-
    to_cnf(and(lit(X),lit(true)), Res).

test(to_cnf2,[true(Res == [[X],[true,false]])]) :-
    to_cnf(and(lit(X), or(lit(true),lit(false))), Res).

test(to_cnf3,[true(Res == [[not(A),B]])]) :-
    to_cnf(implies(not(not(lit(A))),lit(B)), Res).

test(to_cnf4,[true(Res == [[A]])]) :-
    to_cnf(lit(A), Res).

test(to_cnf5,[true(Res == [[A,B]])]) :-
    to_cnf(or(lit(A),lit(B)), Res).

test(to_cnf6,[true(Res == [[A],[B]])]) :-
    to_cnf(and(lit(A),lit(B)), Res).

test(to_cnf7,[true(Res == [[A,B],[A,C]])]) :-
    to_cnf(or(lit(A),and(lit(B),lit(C))), Res).

test(to_cnf8, [true(Res == [[not(A)],[B]])]) :-
    to_cnf(not(or(lit(A),not(lit(B)))), Res).

test(to_cnf9, [true(Res == [[false,not(A)]])]):-
    to_cnf(not(and(not(lit(false)), lit(A))), Res).

test(to_cnf_bt1,[all(Res = [[[not(A),B]]])]) :-
    to_cnf(implies(not(not(lit(A))),lit(B)), Res).

test(to_cnf_bt2,[all(Res = [[[X],[true,false]]])]) :-
    to_cnf(and(lit(X), or(lit(true),lit(false))), Res).

test(to_cnf_bt3,[all(Res = [[[A,B]]])]) :-
    to_cnf(or(lit(A),lit(B)), Res).

test(to_cnf_bt2,[all(Res = [[[A],[B]]])]) :-
    to_cnf(and(lit(A),lit(B)), Res).

:- end_tests(cnf).

:- begin_tests(verify_sat).

test(sat1, [nondet,all(X == [true])]) :-
    to_cnf(and(lit(X),lit(true)), CNF),
    solve(CNF).

test(sat2, [nondet,true(X == true)]) :-
    to_cnf(or(lit(X),and(lit(true),lit(false))), CNF),
    solve(CNF).

test(sat3, [nondet,true([X,Y] == [false,true])]) :-
    to_cnf(or(lit(false),and(not(lit(X)),lit(Y))), CNF),
    solve(CNF).

test(sat4, [nondet,true(X == false; Y == true)]) :-
    to_cnf(or(not(lit(X)),lit(Y)), CNF),
    solve(CNF).

test(sat5, [nondet,true([X,Y] == [true,true])]) :-
    to_cnf(and(lit(X),lit(Y)), CNF),
    solve(CNF).

test(sat6, [nondet,true(X == false)]) :-
    to_cnf(not(lit(X)), CNF),
    solve(CNF).

test(sat7, [nondet,true(X == true)]) :-
    to_cnf(lit(X), CNF),
    solve(CNF).

test(sat8, [nondet]) :-
    to_cnf(not(and(implies(lit(X),not(or(lit(Y),lit(Z)))), or(and(lit(X),lit(Y)), not(lit(Z))))), CNF),
    solve(CNF), !,
    ((X == true, Z == true) ; (Z == true, X == false) ; (Y == false, Z == true) ; (X == true, Y == true)).

test(unsat1, [fail]) :-
    to_cnf(and(not(implies(lit(X),lit(Y))),and(not(lit(X)),lit(Y))), CNF),
    solve(CNF).

test(unsat2, [fail]) :-
    to_cnf(and(not(lit(X)),lit(X)), CNF),
    solve(CNF).

test(unsat3, [fail]) :-
    to_cnf(or(and(not(lit(X)),lit(X)), and(not(lit(Y)),not(not(lit(Y))))), CNF),
    solve(CNF).

test(unsat4, [fail]) :-
    to_cnf(and(and(not(implies(lit(X),lit(Y))),and(not(lit(X)),lit(Y))),and(lit(Z),not(lit(Z)))), CNF),
    solve(CNF).

:- end_tests(verify_sat).
