:- set_prolog_flag(double_quotes, codes).

doubled --> c(A), c(A).
c([]) --> [].
c([A|AS]) --> [A], !, c(AS).
