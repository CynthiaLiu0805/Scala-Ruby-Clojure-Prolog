# F = [X, Y]>>( Y is 2*X ).

general_fizz_buzz(Upper, Rules) :-
    sort(Rules, Rules_sorted),
    between(1, Upper, N),
    findall(
        Word,
        (member(Divisor-Word, Rules_sorted), 0 is N mod Divisor),
        Words
    ),
    (   Words = [], Line = N
    ;   Words = [_|_], atomic_list_concat(Words, Line)
    ),
    writeln(Line),
    false.