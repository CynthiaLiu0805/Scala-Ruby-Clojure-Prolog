hasDivisorLessThanOrEqualTo(_,1) :- !, false.
hasDivisorLessThanOrEqualTo(X,Y) :- 0 is X mod Y, !.
hasDivisorLessThanOrEqualTo(X,Y) :- Z is Y - 1, hasDivisorLessThanOrEqualTo(X,Z).

%part 1
isPrime(2) :- true, !.
isPrime(X) :- not(hasDivisorLessThanOrEqualTo(X,X-1)).

%part 2
%isDigitList(_,[]) :- false.
%isDigitList(X,[X]) :- true, !.    % I comment these out and to achieve isDigitList(X,L_unknown) -> L_unknown=[X]
isDigitList(X,[L|[]]):- X < 10, L is X, !.            % when there is only one digit
isDigitList(X,L) :- 
   Divider is X // 10,                              % drop the last digit
   isDigitList(Divider,L_temp),                       % put the remaining digits to recursion
   Remainder is (X mod 10),                         % find the remainder
   append([Remainder], L_temp ,L).                  % append the remainder to the recursion result, L is the final result



%part 3
isPalindrome(Xs):- isPalindrome(Xs,[]).              % divide the list to two parts
isPalindrome([H|T],Ys):- isPalindrome(T, [H|Ys]).    % move head to the other list
isPalindrome(Xs, Xs):- true, !.                      % if the two lists are same
isPalindrome([X|Xs],Xs):- true, !.                   % if two list are same beside head
                                                     % it is palindrome


%part 4
primePalindrome(X) :- isPrime(X), isDigitList(X,L), isPalindrome(L).  %check if the number is prime first, and convert it to list, and check if the list is palindrome

