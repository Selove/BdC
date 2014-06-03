%operateur
:- op(400, xfy, et).
:- op(400, xfy, ou).
:- op(300, xfy, =>).
:- op(300, xfy, <=>).
:- op(300, xfy, -->>).
:- op(200, fy, non).
:- op(200, fy, <>).
:- op(200, fy, #).

%definir	les propositions
proposition(P) :- member(P,[p,q,r]).

%		les mondes
monde(W) :- member(W, [w0, w1, w2, w3, w4, w5, w6, w7]).

%		la fonction m
m(w0,[]).
m(w1,[p]).
m(w2,[q]).
m(w3,[r]).
m(w4,[p,q]).
m(w5,[p,r]).
m(w6,[q,r]).
m(w7,[p,q,r]).

%		satisfait
satisfait(W, P) :-
     proposition(P),
     monde(W),
     m(W,L),
     member(P,L).


%et
satisfait(W, P et Q) :-
     monde(W),
     satisfait(P,W),
     satisfait(Q,W).
%non
satisfait(non P, W) :-
     monde(W),
     list_w_satisfait(P, LW),
     not(member(W, LW)).
%p est satisfait par un élément de la liste
list_w_satisfait(P, LW) :-
     findall(W,satisfait(W, P), LW).
%ou
satisfait(W,P ou Q) :-
     satisfait(W, non(non P et non Q)).


%implication
satisfait(W, P => Q) :-
     satisfait(W, non (P et non Q)).
%equivalence
satisfait(W, P <=> Q) :-
     satisfait(W, P => Q),
     satisfait(W, Q => P).
%implication stricte
satisfait(W, P -->> Q) :-
     satisfait(W, #(P => Q)).


%si une de mes relations sait que p est vrai alors p est possible.
satisfait(W, <> P) :-
     monde(W),
     un_rel_satisfait(W,P).
%une des relations de W satisfait P
un_rel_satisfait(W, P) :-
     bagof(M, rel(W,M), LM),
     un_satisfait(LM, P).
%un des éléments de la liste satisfait p
un_satisfait([M | LM], P) :-
     (satisfait(M,P) -> true; un_satisfait(LM, P)).

%def de rel (on choisit une relation transitive)
rel(W1, W2) :-
     monde(W1),
     monde(W2),
     m(W1, L1),
     m(W2, L2),
     inclusion(L1, L2).

%necessaire
satisfait(W, # P) :-
     satisfait(W,non(<>(non P))).


%THEOREME : p doit être satisfait dans tous les mondes
theoreme(P) :-
     list_w_satisfait(P, LW),
     findall(W,monde(W), ALLW),
     egal(LW, ALLW).

egal(LW, ALLW) :-
     inclusion(LW, ALLW),
     inclusion(ALLW, LW).

%inclusion(C,E) est l inclusion de C dans E
inclusion([], _). %la liste vide est toujours incluse
inclusion([W|LW], L) :- member(W,L), inclusion(LW,L).




