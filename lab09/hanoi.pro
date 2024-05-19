% hanoi(+N, Skąd, Dokąd, Pom, SekwencjaRuchów)
% Jeśli N = 1: A->C
% jeśli N > 1: k-1 A->B używając C
%              A->C,
%              swap(A, B)

:- use_module(library(lists)).
:- op(500, xfx, na).
:- dynamic hanoiWithMemo/5, runHanoiWithMemo/5, lemat/1, hanoi/5.

hanoi(1, A, B, _, [ A na B ]) :- !.
hanoi(N, A, B, C, Steps) :-
  N > 1, % bo inaczej się pętli  ^
  N1 is N - 1,
  hanoi(N1, A, B, C, NextSteps),
  append(NextSteps, [A na C | LastSteps], Steps),
  hanoi(N1, B, C, A, LastSteps).
  % append na końcu zły, bo niedywajne - brak rekurencji ogonowej

runHanoiWithMemo(N, A, B, C, Steps) :-
  % nowe zmienne, żeby nie zapisywać ustalonych klauzul.
  hanoi(N, X, Y, Z, Steps),
  (A, B, C) = (X, Y, Z).

hanoiWithMemo(1, A, B, _, [A na B]) :- !.
hanoiWithMemo(N, A, B, C, Steps) :- 
  N > 1, 
  N1 is N - 1,
  lemat(hanoi(N1, A, B, C, NextSteps)),
  append(NextSteps, [A na C | LastSteps], Steps),
  hanoi(N1, B, C, A, LastSteps).
  % append na końcu zły, bo niedywajne - brak rekurencji ogonowej)

lemat(A) :-
  ( clause(A) -> true
  ; call(A), asserta((A :- !))
  ).

% w zaliczeniowym: read/write, albo getchar równie dobre.

