sealed trait MixedExpr
case class Const[A](value:Int) extends MixedExpr
case class Neg[A](value:MixedExpr) extends MixedExpr
case class Abs[A](value:MixedExpr) extends MixedExpr
case class Minus[A](value1:MixedExpr, value2:MixedExpr) extends MixedExpr
case class Plus[A](value1:MixedExpr, value2:MixedExpr) extends MixedExpr
case class Times[A](value1:MixedExpr, value2:MixedExpr) extends MixedExpr
case class Exp[A](value1:MixedExpr, value2:MixedExpr) extends MixedExpr
case object TT extends MixedExpr
case object FF extends MixedExpr
case class Band[A](value1:MixedExpr, value2:MixedExpr) extends MixedExpr
case class Bnot[A](value:MixedExpr) extends MixedExpr
case class Bor[A](value1:MixedExpr, value2:MixedExpr) extends MixedExpr


def interpretExpr(value:MixedExpr):Int = value match{
    case Const(value) => value
    case Abs(value) => interpretExpr(value).abs
    case Minus(value1, value2) => interpretExpr(value1)-interpretExpr(value2)
    case Neg(value) => interpretExpr(value)*(-1)
    case Plus(value1, value2) => interpretExpr(value2)+interpretExpr(value1)
    case Times(value1, value2) => interpretExpr(value1)*interpretExpr(value2)
    case Exp(value1, value2) => Math.pow(interpretExpr(value1),interpretExpr(value2)).toInt
}

def interpretBoolean(value:MixedExpr):Boolean = value match {
    case TT => true
    case FF => false
    case Band(value1, value2) => interpretBoolean(value1) && interpretBoolean(value2)
    case Bnot(value) => !interpretBoolean(value)
    case Bor(value1, value2) => interpretBoolean(value1) || interpretBoolean(value2)
}

def interpretMixedExpr(value:MixedExpr): Option[Either[Int, Boolean]]= value match{
    case Const(value) => Some(Left(value.toString.toInt))
    case Abs(value) => Some(Left(interpretExpr(Abs(value))))
    case Minus(value1, value2) => Some(Left(interpretExpr(Minus(value1, value2))))
    case Neg(value) => Some(Left(interpretExpr(Neg(value))))
    case Plus(value1, value2) => Some(Left(interpretExpr(Plus(value1, value2))))
    case Times(value1, value2) => Some(Left(interpretExpr(Times(value1, value2))))
    case Exp(value1, value2) => Some(Left(interpretExpr(Exp(value1, value2))))
    case TT => Some(Right(true))
    case FF => Some(Right(false))
    case Band(value1, value2) => Some(Right(interpretBoolean(Band(value1,value2))))
    case Bnot(value) => Some(Right(interpretBoolean(Bnot(value))))
    case Bor(value1, value2) => Some(Right(interpretBoolean(Bor(value1,value2))))

}

