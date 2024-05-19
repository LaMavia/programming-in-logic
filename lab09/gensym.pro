:- use_module(library(lists)).

:- dynamic(gs/2).

% gensym(+Nazwa, -Symbol)
gensym(Id, Sym) :-
  atom(Id), % exclude variable Names - undefined behaviour
  var(Sym), % validate signature
  getFlag(Id, OldVal),
  NewVal is OldVal + 1,
  setFlag(Id, NewVal),
  create(Id, NewVal, Sym).

% getFlag(+Name, -Value)
getFlag(Id, Val) :-
  ( gs(Id, Val) -> true
  ; Val = 0
  ).

% setFlag(+Name, +Value)
setFlag(Id, Val) :-
  retractall(gs(Id, _)),
  % ^ retract fails, jak nie ma matchujących
  asserta(gs(Id, Val)).
  %     ^ nie ma znaczenia, jaki assert.
  
create(Id, V, Sym) :-
  atom_codes(Id, IdCodes),
  atom_codes(V, VCodes),
  append(IdCodes, VCodes, SymCodes),
  atom_codes(Sym, SymCodes).
