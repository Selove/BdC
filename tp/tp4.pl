%operateurs
:- op(500, fy, <<).
:- op(500, xfy, >>).
:- op(500, xfy, ::).
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
satisfait(P,W) :-
     proposition(P),
     monde(W),
     m(W,L),
     member(P,L).


%et
satisfait(P et Q,W) :-
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
     findall(W,satisfait(P,W), LW).
%ou
satisfait(P ou Q,W) :-
     satisfait( non(non P et non Q), W).


%implication
satisfait(P => Q,W) :-
     satisfait( non (P et non Q), W).
%equivalence
satisfait(P <=> Q,W) :-
     satisfait(P => Q, W),
     satisfait(Q => P, W).
%implication stricte
satisfait(P -->> Q,W) :-
     satisfait(#(P => Q),W).


%si une de mes relations sait que p est vrai alors p est possible.
satisfait(<> P,W) :-
     monde(W),
     un_rel_satisfait(P,W).
%une des relations de W satisfait P
un_rel_satisfait(P,W) :-
     bagof(M, rel(W,M), LM),
     un_satisfait(P, LM).
%un des éléments de la liste satisfait p
un_satisfait(P, [M | LM]) :-
     (satisfait(P,M) -> true; un_satisfait(P, LM)).

%def de rel (on choisit une relation transitive)
rel(W1, W2) :-
     monde(W1),
     monde(W2),
     m(W1, L1),
     m(W2, L2),
     inclus(L1, L2).

%necessaire
satisfait(# P,W) :-
     satisfait(non(<>(non P)),W).

%transforme une liste en conjonction des éléments de la liste
list2conj([A], A).
list2conj([A|LA], A et LC) :-
     list2conj(LA, LC).

%extension aux actions
satisfait(W, <<A>>F) :-
     monde(W),
     action(A, Cond, Sup, Aj),
     rel(W,M),
     list2conj(Cond, Condc),
     satisfait(W, Cond)
     list2conj(Aj, Ajc),
     satisfait(M, Ajc)
     list2conj(Sup, Supc),
     satisfait(M, non (Supc)),
     satisfait(M,F).



%THEOREME : p doit être satisfait dans tous les mondes
theoreme(P) :-
     list_w_satisfait(P, LW),
     findall(W,monde(W), ALLW),
     (egal(LW, ALLW) -> true).

egal(LW, ALLW) :-
     inclusion(LW, ALLW),
     inclusion(ALLW, LW).

%inclusion(C,E) est l inclusion de C dans E
inclusion([], _). %la liste vide est toujours incluse
inclusion([W|LW], L) :- member(W,L), inclusion(LW,L).




