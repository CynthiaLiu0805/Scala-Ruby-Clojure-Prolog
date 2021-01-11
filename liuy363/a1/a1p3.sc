sealed trait VarExpr
case class Const[A](value:Int) extends VarExpr
case class Neg[A](value:VarExpr) extends VarExpr
case class Abs[A](value:VarExpr) extends VarExpr
case class Minus[A](value1:VarExpr, value2:VarExpr) extends VarExpr
case class Plus[A](value1:VarExpr, value2:VarExpr) extends VarExpr
case class Times[A](value1:VarExpr, value2:VarExpr) extends VarExpr
case class Exp[A](value1:VarExpr, value2:VarExpr) extends VarExpr
case class Var[A](value:Symbol) extends VarExpr
case class Subst[A](value1:VarExpr, value2:A, value3:VarExpr) extends VarExpr


def interpretVarExpr(value:VarExpr):Int = value match{
    case Const(value) => value
    case Abs(value) => interpretVarExpr(value).abs
    case Minus(value1, value2) => interpretVarExpr(value1)-interpretVarExpr(value2)
    case Neg(value) => interpretVarExpr(value)*(-1)
    case Plus(value1, value2) => interpretVarExpr(value2)+interpretVarExpr(value1)
    case Times(value1, value2) => interpretVarExpr(value1)*interpretVarExpr(value2)
    case Exp(value1, value2) => Math.pow(interpretVarExpr(value1),interpretVarExpr(value2)).toInt
    case Subst(value1, value2, value3) => interpretVarExpr(subhelp(value1, value2,value3))
}
// helper method to return symbol in String type
def getSymbol(v:VarExpr) : String = v match {
    case Var(value) => value.toString()
    case Const(value) => value.toString()
    case Abs(value) => value.toString()
    case Minus(value1, value2) => interpretVarExpr(Minus(value1, value2)).toString()
    case Neg(value) => value.toString()
    case Plus(value1, value2) => interpretVarExpr(Plus(value1, value2)).toString()
    case Times(value1, value2) => interpretVarExpr(Times(value1, value2)).toString()
    case Exp(value1, value2) => interpretVarExpr(Exp(value1, value2)).toString()
}

// the helper method to transfer the subst to a VarExpr type
def subhelp(value_sub:VarExpr, s_out:Any, value_out:VarExpr): VarExpr = value_sub match {

    case Var(value) => value_out
    case Abs(value) => Abs(value_out)
    case Neg(value) => Neg(value_out)

    case Plus(value_first, value_second) if (s_out.toString==getSymbol(value_first)) => Plus(value_out,value_second)
    case Plus(value_first, value_second) if (s_out.toString==getSymbol(value_second)) => Plus(value_first,value_out)
   
    case Minus(value_first, value_second) if (s_out.toString==getSymbol(value_first)) => Minus(value_out,value_second)
    case Minus(value_first, value_second) if (s_out.toString==getSymbol(value_second)) => Minus(value_first,value_out)
  
    case Times(value_first, value_second) if (s_out.toString==getSymbol(value_first)) => Times(value_out,value_second)
    case Times(value_first, value_second) if (s_out.toString==getSymbol(value_second)) => Times(value_first,value_out)
  
    case Exp(value_first, value_second) if (s_out.toString==getSymbol(value_first)) => Exp(value_out,value_second)
    case Exp(value_first, value_second) if (s_out.toString==getSymbol(value_second)) => Exp(value_first,value_out)
  
    case Subst(value_inner,s2,value_inner2) => subhelp(subhelp(value_inner,s2,value_inner2),s_out,value_out)
}