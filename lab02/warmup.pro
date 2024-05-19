% ostatni(E, L)
% Jest punkt nawrotu po znalezieniu ostatniego
ostatni(E, [ E ]).
ostatni(E, [ _ | L ]) :- ostatni(E, L).

% elem(E, L)
elem(E, [ E | _ ]).
elem(E, [ _ | L ]) :- elem(E, L).

nonElem(E, []).
nonElem(E, [ X | L ]) :- 
  E \= X,
  nonElem(E, L).

% bez pustych punktów nawrotu
% tylko niepusta ma ostatni element
ost(E, [ X | L ]) :-
  ost(L, X, E).

% indeksacja po pierwszym elemencie 
% - zawsze użyjemy drugiej.
ost([], X, X).
ost([ X | L ], _, E) :- ost(L, X, E).


% el
el(E, [ X | L ]) :- el(L, X, E).

el(_, E, E).
el([ X | L ], _, E) :- el(L, X, E).

% indeksacja
nel([], _).
nel([X, L], E) :- 
  X \= E,
  nel(L, E).

