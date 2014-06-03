%operateurs
:- op(400, xfy, et). % /\
:- op(400, xfy, ou). % \/
:- op(300, xfy, =>). % \/
:- op(300, xfy, <=>). % \/
:- op(300, xfy, -->> ). 
:- op(200, fy, <> ). % possible
:- op(200, fy, #). % necessaire
:- op(200, fy, non). % negation

%tp4 : << a :: b :: c >> (P et Q)
:- op(100, fy, <<).
:- op(150, xfy, <<).
:- op(100, xfy, ::).

% definir	les propositions.
proposition(P) :- member(P, [p,q,r]).

%		les mondes
lesMondes(W) :- W = [w0, w1, w2, w3, w4, w5, w6, w7].
monde(W) :- member(W, [w0, w1, w2, w3, w4, w5, w6, w7]).

%		les relations d'accessibilite
%W2 est en relation avec W1
rel(W1, W2) :- 
	monde(W1),
	monde(W2).

%		la fonction de definition des mondes M = <W,R,m>
est(w0, []).
est(w1, [p]).
est(w2, [r]).
est(w3, [q]).
est(w4, [p,r]).
est(w5, [p,q]).
est(w6, [q,r]).
est(w7, [p, r, q]).

%		le predicat satisfait(W,P) <=> W |= P
satisfait(W,P) :-
     proposition(P),
     monde(W),
     est(W,L),
     member(P,L).

satisfait(W, P et Q) :-
     satisfait(W,P),
     satisfait(W,Q).

satisfait(W, non P) :-
     monde(W),
     list_w_satisfait(P, LW),
     not(member(W, LW)).

satisfait(W, P ou Q) :-
     monde(W),
     satisfait(W, non (non P et non Q)).

satisfait(W, P et Q) :-
     satisfait(W,P),
     satisfait(W,Q).

satisfait(W, P => Q) :-
     satisfait(W, non P ou Q).

satisfait(W, P <=> Q) :-
     satisfait(W, (P => Q) et (Q => P)).

%il est impossible que non P soit possible
satisfait(W, # P ) :-
    satisfait(W, non ( <> (non P) ) ).

satisfait(W, P -->> Q) :-
     satisfait(W, # (P => Q)).
	
% je ne sais pas que P est vrai mais je connais quelqu'un qui sait que P est vrai
%donc P est "possible"
satisfait(W, <> P) :-
	monde(W),
        un_rel_satisfait(W, P). %     rel(W, M)

% tp4 : 
satisfait(W, << A >> F) :-
	monde(W),
	rel(W, W2), % \E un monde W2
	est_entrepenable(A, W), % les conditions de A sont satisfaites dans W
	est_effective(A, W2), % les effets de A sont effectifs dans W2
	satisfait(W2, F). % F est satisfait dans W2

% les conditions de A sont satisfaites dans W
est_entrepeunable(A, W) :-
	monde(W),
	action(A, Cond, Suppr, Ajout),
	list2conj(Cond, P),
	satisfait(W, P).

% les effets de A sont effectives dans W
est_effective(A, W) :-
	monde(W),
	action(A, Cond, Suppr, Ajout),
        list2conj(Suppr, SU),
	list2conj(Ajout, AJ),
	satisfait(W, non (SU)),
	satisfait(W, AJ).

%transforme une liste en conjonction des éléments de la liste
list2conj([A], A).
list2conj([A|LA], A et LC) :-
     list2conj(LA, LC)

%P est une formule vraie (dans l'absolue ie dans tous les mondes)
theoreme(P) :-
     list_w_satisfait(P, LM),
     lesMondes(Ws),
     egal(LM, Ws). % inclusion dans les 2 sens

egal(L1, L2) :-
     inclus(L1, L2),
     inclus(L2, L1).

%inclus(C,E) est l inclusion de C dans E
inclus([], _). % la liste vide est incluse dans nptkoi
inclus([C|LC], L) :- member(C,L), inclus(LC,L).

%liste des mondes qui satisfont P
list_w_satisfait(P, LW) :-  
     findall(W, satisfait(W,P), LW).

un_rel_satisfait(W, P) :-
     bagof(M, rel(W,M),LM),
     un_satisfait(LM, P).

un_satisfait([M | LM], P) :-
     (satisfait(M, P) -> true; un_satisfait(LM, P)).

%
