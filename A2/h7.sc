sealed trait ULTerm
case class ULVar(index: Int) extends ULTerm {
  override def toString() = index.toString()
}
case class ULAbs(t: ULTerm) extends ULTerm {
  override def toString() = "lambda . " + t.toString()
}
case class ULApp(t1: ULTerm, t2: ULTerm) extends ULTerm {
  override def toString() = "(" + t1.toString() + ") (" + t2.toString() + ")"
}

// count how many Abs is included in input in order to know how many lambda needed
def countAbs(input:ULTerm):Int = input match {
  case ULAbs(t) => countAbs(t)+1
  case ULApp(t1, t2) => 0
  case ULVar(index) => 0
}

// construct lambda by the result of countAbs
def constructLambda(i:Int):String = {
  if (i>0){
    return "lambda " + getAlphabets(i-1) + " . " +constructLambda(i-1)
  }
  else {
    return ""
  }
}

// remove the Abs, ie. return appvars for val lllappvars = ULAbs(ULAbs(lappvars))
def reduceAbs(input:ULTerm):ULTerm = input match {
  case ULAbs(t) => reduceAbs(t)
  case ULApp(t1, t2) => ULApp(t1, t2)
  case ULVar(index) => ULVar(index)
}

/* use recursion, for ULVar, return the corresponding value of index by using getAlphabets function
   for ULApp, return combination of prettify version of part1 and part2 
   for ULAbs, construct first part(lambda) by taking the number of Abs as input, and the second part prettify as usual*/
def prettify(input:ULTerm):String = input match {
    case ULAbs(t) => constructLambda(countAbs(t)+1).toString() + prettify(reduceAbs(t))
    case ULApp(t1, t2) => "("+ prettify(t1) + ") (" + prettify(t2) + ")"
    case ULVar(index) => getAlphabets(index)
}

// use ASCII to convert the Int to String
def getAlphabets(i:Int):String = {
  if (i<=26)
    return (i+97).toChar.toString() //a is 97 according to ASCII table
  else 
    return getAlphabets(i/26-1)+getAlphabets(i%26-1) //minus one to ensure it start from zero
}


// Shift the numbering of unbound variables
def shift(shiftAmount: Int, t: ULTerm): ULTerm = {
  // Walk through the term and shift all variables with index
  // greater than or equal to c, which is maintained to be
  // the number of variable binders (abstractions) outside the current subterm.
  def walk(currentBinders: Int, t: ULTerm): ULTerm = t match {
    // Check if x is a free variable; that is,
    // if the number x is greater than or equal to
    // the number of variable binders encountered outside this subterm.
    case ULVar(x) if (x >= currentBinders) => ULVar(x+shiftAmount)
    case ULVar(x) => ULVar(x)

    case ULAbs(t) =>
      // We now have one more variable binder outside the subterm.
      // Increment currentBinders and walk into the subterm.
      ULAbs(walk(currentBinders+1, t))

    case ULApp(t1,t2) =>
      // No new variable binders. Just walk into the subterms.
      ULApp(walk(currentBinders,t1),walk(currentBinders,t2))
  }

  // Walk the term and perform the shift of free variables.
  // We begin with 0 variable binders outside.
  walk(0, t)
}

// In our usual syntax, we would write substitution as `t[x := r]`.
// Here we write `substitute(t,x,r)`.
def substitute(t: ULTerm, x: Int, r: ULTerm): ULTerm = {
  // We want to substitute for the free variable with number x.
  // Inside a variable binder (abstraction),
  // the index of all free variables is shifted up by 1.
  // So we must keep track of the number of binders outside the current subterm.
  def walk(currentBinders: Int, t: ULTerm): ULTerm = t match {
    case ULVar(y) if y == x + currentBinders =>
      // y is the xth free variable. Substitute for it,
      // making sure to shift the free variables in r
      // to account for the number of variable binders outside this subterm.
      shift(currentBinders,r)
    
    case ULVar(y) =>
      // Otherwise, y is not the xth free variable;
      // leave it as is.
      ULVar(y)
    
    case ULAbs(t) =>
      // We now have one more variable binder outside the subterm.
      // Increment currentBinders and walk into the subterm.
      ULAbs(walk(currentBinders+1,t))

    case ULApp(t1,t2) =>
      // No new variable binders. Just walk into the subterms.
      ULApp(walk(currentBinders,t1),walk(currentBinders,t2))
  }

  // Walk the term, performing the substitution.
  // We begin with 0 variable binders outside.
  walk(0,t)
}

// We need to know if a term is a value during reduction
// when using call-by-value semantics.
def isValue(t: ULTerm): Boolean = t match {
  case ULAbs(_) => true
  case _ => false
}

// Call-by-value reduction function.
// Performs one step of evaluation, if possible according to the call-by-value rules.
// If no reduction is possible, returns None.
def reduce(t: ULTerm): Option[ULTerm] = t match {

  // Case: the left term is an abstraction, and the right is a value.
  // Then apply the value to the abstraction.
  case ULApp(ULAbs(t),v) if isValue(v) =>
    // When we apply the value to the abstraction,
    // we must shift the value's free variables up by 1 to account
    // for the abstraction's variable binder.
    val r = substitute(t,0,shift(1,v))
    // Then, we need to shift the result back.
    // Since the abstraction's variable is now "used up".
    Some(shift(-1,r))

  // Case: the left term is a value, then try to reduce the right term.
  case ULApp(v,t) if isValue(v) =>
    reduce(t) match {
      case Some(r) => Some(ULApp(v,r))
      case None => None
    }

  // Case: the left term is not a value (not an abstraction.)
  // Try to reduce it.
  case ULApp(t1,t2) =>
    reduce(t1) match {
      case Some(r1) => Some(ULApp(r1,t2))
      case None => None
    }
    
  case _ => None
}

// Evaluation just repeatedly applies reduce,
// until we reach None (signifying reduction failed.)
def evaluate(t: ULTerm): ULTerm = reduce(t) match {
  case None => t
  case Some(r) => evaluate(r)
}