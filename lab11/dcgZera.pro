:- set_prolog_flag(double_quotes, codes).

zera, [C] --> "0", zera, [C], {cyfra(C)}, !.
                                        % ^ żadkie, poprawne końcowe odcięcie :D
zera --> [].

lookahead(P), [P] --> [P].
