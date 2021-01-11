sealed trait Expr
case class Const[A](value:Int) extends Expr
case class Neg[A](value:Expr) extends Expr
case class Abs[A](value:Expr) extends Expr
case class Minus[A](value1:Expr, value2:Expr) extends Expr
case class Plus[A](Tvalue1:Expr, value2:Expr) extends Expr
case class Times[A](value1:Expr, value2:Expr) extends Expr
case class Exp[A](value1:Expr, value2:Expr) extends Expr
