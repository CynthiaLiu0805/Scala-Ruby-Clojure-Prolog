isMixedExpr(constE(A)).
isMixedExpr(negE(A)).
isMixedExpr(absE(A)).
isMixedExpr(minusE(A,B)).
isMixedExpr(plusE(A,B)).
isMixedExpr(timesE(A,B)).
isMixedExpr(expE(A,B)).
isMixedExpr(tt).
isMixedExpr(ff).
isMixedExpr(band(A,B)).
isMixedExpr(bor(A,B)).
isMixedExpr(bnot(A,B)).

interpretMixedExpr(constE(A),X) :- X is A.
interpretMixedExpr(negE(A),X) :- interpretMixedExpr(A,X1), X is X1*(-1).
interpretMixedExpr(absE(A),X) :- interpretMixedExpr(A,X1), X is abs(X1).
interpretMixedExpr(minusE(A,B),X) :- interpretMixedExpr(A,X1),interpretMixedExpr(B,X2), X is X1-X2.
interpretMixedExpr(plusE(A,B),X) :- interpretMixedExpr(A,X1),interpretMixedExpr(B,X2), X is X1+X2.
interpretMixedExpr(timesE(A,B),X) :- interpretMixedExpr(A,X1),interpretMixedExpr(B,X2), X is X1*X2.
interpretMixedExpr(expE(A,B),X) :- interpretMixedExpr(A,X1),interpretMixedExpr(B,X2), X is X1**X2.

interpretMixedExpr(tt,X) :- X = true.
interpretMixedExpr(ff,X) :- X = false.
interpretMixedExpr(bnot(A),X) :- interpretMixedExpr(A,Y), Y==true -> X = false.
interpretMixedExpr(bnot(A),X) :- interpretMixedExpr(A,Y), Y==false -> X = true.
interpretMixedExpr(bor(A,B),X) :- interpretMixedExpr(A,Y1), interpretMixedExpr(B,Y2) -> X = Y1;Y2 .
interpretMixedExpr(band(A,B),X) :- interpretMixedExpr(A,Y1) , interpretMixedExpr(B,Y2),Y1==true, Y2==true -> X = true.
interpretMixedExpr(band(A,B),X) :- interpretMixedExpr(A,Y1) , interpretMixedExpr(B,Y2), Y1==true, Y2==false -> X = false.
interpretMixedExpr(band(A,B),X) :- interpretMixedExpr(A,Y1) , interpretMixedExpr(B,Y2),Y1==false, Y2==false -> X = false.
interpretMixedExpr(band(A,B),X) :- interpretMixedExpr(A,Y1) , interpretMixedExpr(B,Y2),Y1==false, Y2==true -> X = true.