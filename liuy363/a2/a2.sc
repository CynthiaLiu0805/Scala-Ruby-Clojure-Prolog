import $file.a2_ulterm, a2_ulterm._ 

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
    return true
  }
}
  catch {
    case _: Throwable => false
  }

// typeOf(t,emptyEnv) returns a type for t
def typeOf(t: STTerm,l:List[STType]):Option[STType] = t match {
  case STVar(index) => None
  case STTrue | STFalse => Some(STBool)
  case STIsZero(t) => if (typeOf(t,List[STType]())==Some(STNat)) Some(STBool) else None // if typeOf(t) is nat then overall type is bool
  case STSuc(t) => if (typeOf(t,List[STType]())==Some(STNat)) Some(STNat) else None // if typeOf(t) is nat then overall type is nat
  case STZero => Some(STNat)
  case STTest(STTrue|STFalse, t2, t3) => typeOf(t2,List[STType]()) // first term need to be a bool
  case STTest(_, t2, t3) => None
  case STAbs(t,term) =>
  term match {    
    case STVar(index) if ((l:+(t)).length < index) =>  None   // when it is a free variable
    case STVar(index) if (((l:+(t)).lift(index).get)==t) => Some(STFun(t,STNat)) // when the index(index parameter of the STVar) at the list is of type t
    case STApp(t1, t2) => ((l:+(t)).lift(0).get) match {case STFun(dom,codom) if (dom==t) => Some(STNat)
                                                        case _ => None}    // when it has the same type as doman of STFun
    case _ => Some(STFun(t,typeOf(term,l:+(t)).get))
  }
  case STApp(t1, t2) => (typeOf(t1,List[STType]())::l)(0) match {
    case Some(STFun(type1,type2)) if Some(type1)==typeOf(t2,List[STType]()) => Some(type2)
    case _ => None 
  }  
}

def eraseTypes(input:STTerm): ULTerm = input match {
  // translate directly
  case STVar(index) => ULVar(index)
  case STTrue => ULAbs(ULAbs(ULVar(1)))
  case STFalse => ULAbs(ULAbs(ULVar(0)))
  case STZero => ULAbs(ULAbs(ULVar(0)))
  // using recursion
  case STTest(t1, t2, t3) => ULAbs(ULAbs(ULAbs(ULApp(ULApp(eraseTypes(t1),eraseTypes(t2)),eraseTypes(t3)))))
  case STSuc(t) => ULApp(ULAbs(ULAbs(ULAbs(ULApp(ULVar(1),ULApp(ULApp(ULVar(2),ULVar(1)),ULVar(0)))))),eraseTypes(t))
  case STApp(t1, t2) => ULApp(eraseTypes(t1),eraseTypes(t2))
  case STAbs(t, term) => ULAbs(eraseTypes(term))
  case STIsZero(t) => ULAbs(ULApp(ULApp(eraseTypes(t),ULAbs(eraseTypes(STFalse))), eraseTypes(STTrue)))
}
