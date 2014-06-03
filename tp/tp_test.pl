:- op(500,yfx,true_in).
:- op(450,xfx,possible_in).
:- op(500,yfx,give).
:- op(450,xfx,from).

at(home) true_in s0.
not_have(milk) true_in s0.

have(milk) true_in result(buy(milk),S) :-
    buy(milk) possible_in S.
    possible(buy(_)) true_in _.
at(L) true_in result(goto(L),S) :-
    member(L,[home,grocery]),
    goto(L) possible_in S.
    not_at(L) true_in S :-
    at(H) true_in S,
    H \= L.

goto(L) possible_in S :-
    not_at(L) true_in S.
buy(_) possible_in S :-
    at(grocery) true_in S,
    possible(buy(_)) true_in S.

resulter([ ]) from S give S.
resulter([A | P]) from S0 give SF :-
    resulter(P) from result(A,S0) give SF.
