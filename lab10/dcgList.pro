
% DCG:
% head --> body.
% ^ atomy  ^ termy oddzielone ,

:- set_prolog_flag(double_quotes, codes).

% (ala,((),(basia)))
lista --> "(", elems, ")".
elems --> [].
elems --> elem, resztaElems.
elem  --> ident.
elem  --> lista.
resztaElems --> [].
resztaElems --> ",", elem, resztaElems.
ident --> litera, resztaLiter.
resztaLiter --> [].
resztaLiter --> litera, resztaLiter.
litera --> "a".
litera --> "b".
% litera(L) --> [L], {is_letter(L)}.
% is_letter(L) :- 0'a =< L, L =< 0'z.

max(X,Y,Z) :- (X =< Y -> Z=Y; Z=X).


