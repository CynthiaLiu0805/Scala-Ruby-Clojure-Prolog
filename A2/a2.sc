import java.nio.channels.NonWritableChannelException
import $file.a2_ulterm, a2_ulterm._ //, $file.h7, h7._

sealed trait STType
case object STNat extends STType {
  override def toString() = "nat"
}
case object STBool extends STType {
  override def toString() = "bool"
}
// Functions have a domain type and a codomain type.
case class STFun(dom: STType, codom: STType) extends STType {
  override def toString() = "(" + dom.toString + ") -> (" + codom.toString + ")"
}
// Example use: the type "nat -> bool" is written STFun(STNat,STBool)

sealed trait STTerm
case class STVar(index: Int) extends STTerm
case class STApp(t1: STTerm,t2: STTerm) extends STTerm
case class STAbs(t: STType,term: STTerm) extends STTerm

case object STZero extends STTerm
case class STSuc(t: STTerm) extends STTerm
case class STIsZero(t: STTerm) extends STTerm

case object STTrue extends STTerm
case object STFalse extends STTerm
case class STTest(t1: STTerm,t2: STTerm,t3: STTerm) extends STTerm

def typecheck(input:STTerm):Boolean= try {
  if (typeOf(input, List[STType]())==None) {
    return false
  }
  else {
    print(input)
    return true
  }
}
  catch {
    case _: Throwable => false
    //case e: IndexOutOfBoundsException => false
  }

// typeOf(t,emptyEnv) returns a type for t
def typeOf(t: STTerm,l:List[STType]):Option[STType] = t match {
  case STVar(index) => None
  case STTrue | STFalse => Some(STBool)
  case STIsZero(t) => if (typeOf(t,List[STType]())==Some(STNat)) Some(STBool) else None

  case STSuc(t) => if (typeOf(t,List[STType]())==Some(STNat)) Some(STNat) else None
  //case STSuc(t) if (typeOf(t,List[STType]())!=Some(STNat)) => None

  case STZero => Some(STNat)

  case STTest(STTrue|STFalse, t2, t3) => typeOf(t2,List[STType]())
  // case STTest(STFalse, t2, t3) => typeOf(t3,List[STType]())
  case STTest(_, t2, t3) => None

  case STAbs(t,term) => //t::l; 
  print(l);
  print(term);
  print((l:+t));
  print(t);
  print((l:+(t)));


  // typeOf(STAbs(t,term),l); 
  term match {    
    case STVar(index) if ((l:+(t)).length < index) => print((t::l).length< index); None
    case STVar(index) if (((l:+(t)).lift(index).get)==t) => Some(STFun(t,STNat))
    case STApp(t1, t2) => ((l:+(t)).lift(0).get) match {case STFun(dom,codom) if (dom==t) => print(t); Some(STNat)
                                             case _ => None}
    // case STVar(index)  =>   print((l.+:(t)),t,index); Some(STFun(t,STNat))
    case _ => Some(STFun(t,typeOf(term,l:+(t)).get))
  }

  case STApp(t1, t2) => (typeOf(t1,List[STType]())::l)(0) match {
    case Some(STFun(type1,type2)) if Some(type1)==typeOf(t2,List[STType]()) => print((typeOf(t1,List[STType]())::l)(0));print(type2);Some(type2)
    case _ => None 
  }  
}
//STAbs(STFun(STNat, STNat),STAbs(STNat,STApp(STVar(1), STVar(0))))
//true
//STAbs(STNat, STAbs(STBool, STVar(1)))
//(λ x: Nat | (λ y: Bool | x))
//eraseTypes(STSuc(STSuc(STZero)))
//res0: ULTerm = ULApp(
  //ULAbs(ULAbs(ULAbs(ULApp(ULVar(1), ULApp(ULApp(ULVar(2), ULVar(1)), ULVar(0)))))),
 // ULApp(ULAbs(ULAbs(ULAbs(ULApp(ULVar(1), ULApp(ULApp(ULVar(2), ULVar(1)), ULVar(0)))))), ULAbs(ULAbs(ULVar(0))))
//)

def eraseTypes(input:STTerm): ULTerm = input match {
  case STVar(index) => ULVar(index)
  case STTrue => ULAbs(ULAbs(ULVar(1)))
  case STFalse => ULAbs(ULAbs(ULVar(0)))
  case STZero => ULAbs(ULAbs(ULVar(0)))
  case STTest(t1, t2, t3) => ULAbs(ULAbs(ULAbs(ULApp
  (ULApp(eraseTypes(t1),eraseTypes(t2)),eraseTypes(t3)))))
   //ULAbs(ULAbs(ULAbs(ULApp(ULApp(ULVar(2),ULVar(1)),ULVar(0)))))//ULApp(ULAbs(ULAbs(ULAbs(ULApp(ULApp(ULVar(2),ULVar(1)),ULVar(0)))))) //ULAbs(ULAbs(ULAbs(ULVar(2),ULVar(1)),ULVar(0)))
  case STSuc(t) => ULApp(ULAbs(ULAbs(ULAbs(ULApp(ULVar(1),ULApp(ULApp(ULVar(2),ULVar(1)),ULVar(0)))))),eraseTypes(t))
  case STApp(t1, t2) => ULApp(eraseTypes(t1),eraseTypes(t2))
  case STAbs(t, term) => ULAbs(eraseTypes(term))
  case STIsZero(t) => ULAbs(ULApp(ULApp(eraseTypes(t),
   ULAbs(eraseTypes(STFalse))), eraseTypes(STTrue)))
   //ULAbs(ULApp(ULApp(ULVar(0), ULAbs(erase(STFalse)), erase(STTrue)))
}
