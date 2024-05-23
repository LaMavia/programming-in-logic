% E -> E ± T | T.
% T -> T */ F | F.
% F -> liczba.
% problem, bo lewostronna rekursja. Inaczej:
wyr(W) --> wyr(max, W).
wyr(M, W) --> liczba(L), reszta(L, M, W).
reszta(L, ?, ...) --> opAdd(Op), !, wyr(min, P), reszta(...).
%                                 ^ nie wczytujemy całego
reszta() --> opMul(Op), !, liczba(P), {NL =.. [Op, L, P]}, reszta(NL, ).
%                          ^ bo mnożenie mocniej wiąże
reszta() --> [].

