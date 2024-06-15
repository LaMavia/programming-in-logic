lubi(jan, piwo).              
lubi(ewa, sok).
lubi(jan, golonka).         
lubi(ewa, wino).
lubi(adam, wino).          
lubi(kasia, piwo).
lubi(adam, ciasto).       
lubi(basia, herbata).


% coLubi(+Kto, -Lista) == Lista 
% = zbiór wszystkich rzeczy lubianych przez osobę Kto
coLubi(X, S) :-
  setof(Y, lubi(X, Y), S).


% ktoLubi(+Co, -ListaOsób)
ktoLubi(X, S) :-
  setof(Y, lubi(Y, X), S).

% osoby(-Osoby)
osoby(S) :-
  setof(X, Y^lubi(X, Y), S).

% lubiane(-Lista)
lubiane(S) :-
  setof(Y, X^lubi(X, Y), S).

% ktoCoLubi(-ListaKtoCo)
ktoCoLubi(S) :-
  setof((X, YS), coLubi(X, YS), S).

% coKtoLubi(-ListaCoKto)
coKtoLubi(S) :-
  setof((Y, XS), ktoLubi(Y, XS), S).
