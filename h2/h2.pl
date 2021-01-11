hasDivisorLessThanOrEqualTo(_,1) :- !, false.
hasDivisorLessThanOrEqualTo(X,Y) :- 0 is X mod Y, !.
hasDivisorLessThanOrEqualTo(X,Y) :- Z is Y - 1, hasDivisorLessThanOrEqualTo(X,Z).
/*If Y is 1, end the search (don't backtrack to look for other proofs) and fail (search turns up false).
Otherwise, if Y does divide X, end the search immediately (don't backtrack looking for other proofs) (and the search will return true).
Otherwise, check if there is a divisor of X which is less than Y - 1.*/

%part 1
isPrime(2) :- true, !.
isPrime(X) :- not(hasDivisorLessThanOrEqualTo(X,X-1)).


%part 2
dropLast([_],[]).                                   % The last element is dropped.
dropLast([H|T],[H|T2]) :-
  % Aside from the base case above, the lists must match.
  dropLast(T,T2).

reverse(Xs,Ys) :- reverse(Xs,[],Ys).
reverse([],A,A).
reverse([H|T],R,A) :- reverse(T,[H|R],A).


reverse_digits(N, M) :-
    (   integer(N) ->
        reverse_digits(N, 0, M)
    ;   integer(M),
        reverse_digits(M, 0, N)
    ).

reverse_digits(0, M, M) :- !.
reverse_digits(N, M0, M) :-
    N > 0,
    R is N div 10,
    M1 is M0 * 10 + N mod 10,
    reverse_digits(R, M1, M).



isDigitList(_,[]) :- false.
isDigitList(X,[X]) :- true, !.    % when there is only one digit and one element in the list
isDigitList(X,L) :- last(L,F), F is X mod 10, dropLast(L,Lt), Y is div(X,10), isDigitList(Y,Lt).
                    %get the last digit of number(which is the remainder of X mod 10), check if F is the last element of list
                    % drop the last element of list, call it Lt, calculate X//10 call it Y, recursion (Y,Lt)


%part 3
% palin(Xs) :- reverse(Xs,Xs).  
isPalindrome(Xs):- isPalindrome(Xs,[]).              % divide the list to two parts
isPalindrome([H|T],Ys):- isPalindrome(T, [H|Ys]).    % move head to the other list
isPalindrome(Xs, Xs):- true, !.                      % if the two lists are same
isPalindrome([X|Xs],Xs):- true, !.                   % if two list are same beside head
                                                     % it is palindrome


%part 4
primePalindrome(X) :- isPrime(X), intToList(X,L), isPalindrome(L).

intToList(X,[L|[]]):- X < 10, L is X, !.            % when there is only one digit
intToList(X,L):-
   Divider is X // 10,                              % drop the last digit
   intToList(Divider,L_temp), write(L_temp),        % put the remaining digits to recursion
   Remainder is (X mod 10),                         % find the remainder
   append(L_temp,[Remainder] ,L).                   % append the recursion result to the remainder, L is the final result