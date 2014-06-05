%univers initial
init(E) :- E=[lieu(robot)=a, libre(main(robot)), lieu(boite)=b].

%but
but(B) :- B=[lieu(robot)=b, lieu(boite)=a, libre(main(robot))].

%actions
action(aller(robot,X,Y), [lieu(robot) = X], [lieu(robot) = X], [lieu(robot) = Y]) :-
    member(X,[a,b]), member(Y,[a,b]), X \= Y.

action(prendre(robot,O), [lieu(robot) = L, lieu(O) = L, libre(main(robot))], [libre(main(robot)), lieu(O)=L], [lieu(O)=main(robot)]) :- 
    member(L,[a,b]), member(O,[boite]).

action(poser(robot,O), [lieu(robot) = L, lieu(O) = main(robot)], [lieu(O)=main(robot)], [lieu(O) = L, libre(main(robot))]) :- member(L, [a,b]), member(O, [boite]).

%transitions entre etats
transition(A,E,F) :- action(A, C, S, AJ), verifcond(C,E), suppress(S, E, EI), ajouter(AJ,EI,F).

%verifcond(C,E) est l inclusion de C dans E
verifcond([], _). %la liste vide est toujours incluse
verifcond([C|LC], L) :- member(C,L), verifcond(LC,L).

%suppression
suppress([],L,L).
suppress([X|Y], Z, T) :- delete(Z,X,U), suppress(Y,U,T).

%ajout
ajouter(AJ,E,F) :- union(AJ,E,F). %inutile

%generation de plan
genere(E,F,[A],1):-
    transition(A,E,F).

genere(EI,EF,[ACT|PLAN],M):-
    M > 1, transition(ACT,EI,E), N is M-1, between(1,N,P), genere(E,EF,PLAN,P).

%planification
%situation du TP
planifier(Plan, T) :-
    init(E),
    but(B),
    nl,
    write(' Profondeur limite : '),
    read(Prof),
    nl,
    genere(E,F,Plan,Prof),
    verifcond(B,F),
    cputime(T).

%planification
%situation du TP
planifier(Plan, T, Init, But) :-
    nl,
    write(' Profondeur limite : '),
    read(Prof),
    nl,
    genere(E,F,Plan,Prof),
    verifcond(B,F),
    cputime(T).


