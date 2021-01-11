
isVarExpr(constE(A)).
isVarExpr(negE(A)).
isVarExpr(absE(A)).
isVarExpr(minusE(A,B)).
isVarExpr(plusE(A,B)).
isVarExpr(timesE(A,B)).
isVarExpr(expE(A,B)).
isVarExpr(var(A)).
isVarExpr(subst(A,B,C)).


interpretVarExpr(constE(A),X) :- X is A.
interpretVarExpr(negE(A),X) :- interpretVarExpr(A,X1), X is X1*(-1).
interpretVarExpr(absE(A),X) :- interpretVarExpr(A,X1), X is abs(X1).
interpretVarExpr(minusE(A,B),X) :- interpretVarExpr(A,X1),interpretVarExpr(B,X2), X is X1-X2.
interpretVarExpr(plusE(A,B),X) :- interpretVarExpr(A,X1),interpretVarExpr(B,X2), X is X1+X2.
interpretVarExpr(timesE(A,B),X) :- interpretVarExpr(A,X1),interpretVarExpr(B,X2), X is X1*X2.
interpretVarExpr(expE(A,B),X) :- interpretVarExpr(A,X1),interpretVarExpr(B,X2), X is X1**X2.
% when substitution exists, call helper predicate to simplify the substitution, then put the result into interpretVarExpr predicate
interpretVarExpr(subst(A,B,C),X) :- subhelp(A,B,C,R), interpretVarExpr(R,X).
interpretVarExpr(var(A),X) :- interpretVarExpr(A,X1),X is X1.

subhelp(var(A),A,C,R) :- R=C.
subhelp(absE(A),C,D,R) :- R=absE(D).
subhelp(negE(A),C,D,R) :- R=negE(D).

subhelp(plusE(A,B),C,D,R) :- A==var(C) -> R = plusE(D,B).
subhelp(plusE(A,B),C,D,R) :- B==var(C) -> R = plusE(A,D).

subhelp(minusE(A,B),C,D,R) :- A==var(C) -> R = minusE(D,B).
subhelp(minusE(A,B),C,D,R) :- B==var(C) -> R = minusE(A,D).

subhelp(timesE(A,B),C,D,R) :- A==var(C) -> R = timesE(D,B).
subhelp(timesE(A,B),C,D,R) :- B==var(C) -> R = timesE(A,D).

subhelp(expE(A,B),C,D,R) :- A==var(C) -> R = expE(D,B).
subhelp(expE(A,B),C,D,R) :- B==var(C) -> R = expE(A,D).

subhelp(subst(A1,A2,A3),B,C,R) :- subhelp(A1,A2,A3,R1),subhelp(R1,B,C,R).