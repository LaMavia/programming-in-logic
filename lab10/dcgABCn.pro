:- set_prolog_flag(double_quotes, codes).

an(N) --> chs("a", N), chs("b", N), chs("c", N).
chs(C, N) --> C, !, chs(C, N1), {N is N1 + 1}.
chs(_, 0) --> [].
