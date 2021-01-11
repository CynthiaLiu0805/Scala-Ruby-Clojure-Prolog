isExpr(constE(A)).
isExpr(negE(A)).
isExpr(absE(A)).
isExpr(minusE(A,B)).
isExpr(plusE(A,B)).
isExpr(timesE(A,B)).
isExpr(expE(A,B)).

interpretExpr(constE(A),X) :- X is A.
interpretExpr(negE(A),X) :- interpretExpr(A,X1),X is X1*(-1).
interpretExpr(absE(A),X) :- interpretExpr(A,X1),X is abs(X1).
interpretExpr(minusE(A,B),X) :- interpretExpr(A,X1),interpretExpr(B,X2), X is X1-X2.
interpretExpr(plusE(A,B),X) :- interpretExpr(A,X1),interpretExpr(B,X2), X is X1+X2.
interpretExpr(timesE(A,B),X) :- interpretExpr(A,X1),interpretExpr(B,X2), X is X1*X2.
interpretExpr(expE(A,B),X) :- interpretExpr(A,X1),interpretExpr(B,X2), X is X1**X2.

