:- set_prolog_flag(double_quotes, codes).

wr(W) --> war(A), operator(Op), war(B), { W =.. [Op, A, B] }.

wr2(R) --> wyr(L), reszta(L, R).
reszta(L, lt(L, P)) --> "<", !, wyr(P). % itp
wr3(R) --> wyr(L), oper(L, P, R), wyr(P).
oper(L, P, lt(L, P)) --> "<", !. % itp
operator(lt) --> "<", !. % itp
