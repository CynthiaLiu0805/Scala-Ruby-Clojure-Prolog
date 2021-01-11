sealed trait Expr
case class Const[A](value:Int) extends Expr
case class Neg[A](value:Expr) extends Expr
case class Abs[A](value:Expr) extends Expr
case class Minus[A](value1:Expr, value2:Expr) extends Expr
case class Plus[A](Tvalue1:Expr, value2:Expr) extends Expr
case class Times[A](value1:Expr, value2:Expr) extends Expr
case class Exp[A](value1:Expr, value2:Expr) extends Expr



def interpretExpr(value:Expr):Int = value match{
    case Const(value) => value
    case Abs(value) => interpretExpr(value).abs
    case Minus(value1, value2) => interpretExpr(value1)-interpretExpr(value2)
    case Neg(value) => interpretExpr(value)*(-1)
    case Plus(value1, value2) => interpretExpr(value2)+interpretExpr(value1)
    case Times(value1, value2) => interpretExpr(value1)*interpretExpr(value2)
    case Exp(value1, value2) => Math.pow(interpretExpr(value1),interpretExpr(value2)).toInt
}


