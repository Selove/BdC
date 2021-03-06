%univers initiaux
	%cube1 sur cube2 sur table b et robo1 en b
init(EI) :- EI = [position(robo1) = b, position(robo3) = c, position(robo2) = b, 
		position(cube1) = b, position(cube2) = b, 
		accessible(cube1), 
		libre(main(robo1)), libre(main(robo2)), libre(main(robo3)), 
		sur(cube1,cube2), sur(cube2,table(b))].
	%cube1 sur cube2 sur table b 
init1(EI) :- EI = [position(robo1) = b, position(robo3) = c, position(robo2) = b, 
		position(cube1) = b, position(cube2) = b, 
		accessible(cube1), 
		libre(main(robo1)), libre(main(robo2)), libre(main(robo3)), 
		sur(cube1,cube2), sur(cube2,table(b))].
	%cube2 sur tableb et cube1 sur tablec
init2(EI) :- EI = [position(robo1) = b, position(robo3) = c, position(robo2) = b, 
		position(cube1) = c, position(cube2) = b, 
		accessible(cube1), accessible(cube2),
		libre(main(robo1)), libre(main(robo2)), libre(main(robo3)), 
		sur(cube1,table(c)), sur(cube2,table(b))].

initInv(EI) :- EI = [position(robo2) = b, 
		position(cube1) = b, position(cube2) = b, 
		accessible(cube1), 
		libre(main(robo2)), 
		sur(cube1,cube2), sur(cube2,table(b))].
init2Dep(EI) :- EI = [position(robo1) = b, position(robo2) = b,
		position(cube1) = b, position(cube2) = b, 
		accessible(cube1), 
		libre(main(robo2)), libre(main(robo1)),
		sur(cube1,cube2), sur(cube2,table(b))].
initDepl(EI) :- EI = [position(robo1) = b,
		position(cube1) = b, position(cube2) = b, 
		accessible(cube1), accessible(cube2), 
		libre(main(robo1)),
		sur(cube1,table(b)), sur(cube2,table(b))].
%buts
	%cube1 sur cube2 sur table c et robo1 en c 
but(BUT):- BUT = [position(robo1) = c, position(cube1) = c, position(cube2) = c, 
		accessible(cube1), 
		sur(cube1,cube2), sur(cube2,table(c))].
	%cube2 sur cube1 sur table b 
but1(BUT) :- BUT = [position(robo1) = b, position(robo3) = c, position(robo2) = b, 
		position(cube1) = b, position(cube2) = b, 
		accessible(cube2), 
		libre(main(robo1)), libre(main(robo2)), libre(main(robo3)), 
		sur(cube2,cube1), sur(cube1,table(b))].
	%cube1 sur cube2 sur table c et robo1 en c 
but2(BUT) :- BUT = [position(robo1) = c, position(robo3) = c, position(robo2) = b, 
		position(cube1) = c, position(cube2) = c, 
		accessible(cube1), 
		libre(main(robo1)), libre(main(robo2)), libre(main(robo3)), 
		sur(cube1,cube2), sur(cube2,table(c))].
butInv(BUT) :- BUT = [position(robo2) = b, 
		position(cube1) = b, position(cube2) = b, 
		accessible(cube2), 
		libre(main(robo2)),
		sur(cube2,cube1), sur(cube1,table(b))].
but2Dep(BUT) :- BUT = [position(robo2) = b, position(robo1) = c,
		position(cube1) = c, position(cube2) = c, 
		accessible(cube2), accessible(cube1),
		libre(main(robo2)), libre(main(robo1)),
		sur(cube1,table(c)), sur(cube2,table(c))].
butDepl(BUT) :- BUT = [position(robo1) = c,
		position(cube1) = c, position(cube2) = c, 
		accessible(cube2), accessible(cube1),
		libre(main(robo1)),
		sur(cube1,table(c)), sur(cube2,table(c))].

%actions
action( aller_a_vide(R,Ld,La),
	[position(R) = Ld, libre(main(R))],
	[position(R) = Ld],
	[position(R) = La] ) :-
	member(Ld,[a,b,c]), member(La,[a,b,c]), member(R,[robo1]), Ld \= La.

action( transporter(R,Ld,La,O),
	[position(R) = Ld, position(O) = Ld, lieu(O) = main(R)],
	[position(R) = Ld, position(O) = Ld],
	[position(R) = La, position(O) = La] ) :-
	member(Ld,[a,b,c]), member(La,[a,b,c]), member(R,[robo1]), member(O,[cube1,cube2]), Ld \= La.

action( attraper(R,O,L),
	[position(R) = L, position(O) = L, sur(O,table(L)), accessible(O), libre(main(R))],
	[sur(O,table(L)), accessible(O), libre(main(R))],
	[lieu(O) = main(R)] ) :-
	member(L,[a,b,c]), member(R,[robo1, robo2, robo3]), member(O,[cube1,cube2]).

action( saisir(R,O,L),
	[position(R) = L, position(O) = L, sur(O,Osous), accessible(O), libre(main(R))],
	[sur(O,Osous), accessible(O), libre(main(R))],
	[lieu(O) = main(R), accessible(Osous)] ) :-
	member(L,[a,b,c]), member(R,[robo2, robo3]), member(O,[cube1,cube2]), member(Osous,[cube1,cube2]), O \= Osous.

action( deposer(R,O,L),
	[position(R) = L, position(O) = L, lieu(O) = main(R)],
	[lieu(O) = main(R)],
	[sur(O,table(L)), accessible(O), libre(main(R))] ) :-
	member(L,[a,b,c]), member(R,[robo1, robo2, robo3]), member(O,[cube1,cube2]).

action( empiler(R,Osur,Osous,L),
	[position(R) = L, position(Osur) = L, lieu(Osur) = main(R), accessible(Osous)],
	[lieu(Osur) = main(R), accessible(Osous)],
	[sur(Osur,Osous), accessible(Osur), libre(main(R))] ) :-
	member(L,[a,b,c]), member(R,[robo2, robo3]), member(Osur,[cube1,cube2]), member(Osous,[cube1,cube2]), Osur \= Osous.

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

%planification pour le cas du TP
planifier(Plan, Temps, F) :-
    init(E),
    but(B),
    nl,
    write(' Profondeur limite : '),
    read(Prof),
    nl,
    genere(E,F,Plan,Prof),
    verifcond(B,F),
    cputime(Temps).

%planification parametree
%inverser la pile en restant sur la meme table avec uniquement robo2
planifierInverser(Plan, Temps) :-
    initInv(E),
    butInv(B),
    nl,
    write(' Profondeur limite : '),
    read(Prof),
    nl,
    genere(E,F,Plan,Prof),
    verifcond(B,F),
    cputime(Temps).

%planification parametree
%dépiler les 2 cubes puis les deplacer de la table b vers la table c avec robo2 et robo1
planifier2Dep(Plan, Temps, F) :-
    init2Dep(E),
    but2Dep(B),
    nl,
    write(' Profondeur limite : '),
    read(Prof),
    nl,
    genere(E,F,Plan,Prof),
    verifcond(B,F),
    cputime(Temps).

%planification parametree
%deplacer les 2 cubes de la table b vers la table c avec robo2 et robo1, les 2 cubes, etant au debut et a la fin a meme la table 
planifierDeplacer(Plan, Temps, F) :-
    initDepl(E),
    butDepl(B),
    nl,
    write(' Profondeur limite : '),
    read(Prof),
    nl,
    genere(E,F,Plan,Prof),
    verifcond(B,F),
    cputime(Temps).
