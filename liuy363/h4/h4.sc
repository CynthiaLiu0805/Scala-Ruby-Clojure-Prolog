sealed trait Stream[+A]
case object SNil extends Stream[Nothing]
case class Cons[A](a: A, f: Unit => Stream[A]) extends Stream[A]

//part one
def filter[A](fun: A => Boolean, s:Stream[A]):Stream[A]=s match {
    case Cons(a, f) => fun match {
        case fun if fun(a) => Cons(a,_ =>filter(fun,f()))
        case fun if !fun(a) => filter(fun,f())
        case _ => SNil
    }
    case SNil => SNil
}


//part two
def zip[A](s:Stream[A],t:Stream[A]):Stream[(A,A)] = s match{
    case Cons(a, f) => t match {
        case Cons(b,g) => Cons((a,b),_ => zip(f(),g()))
        case SNil => SNil
    }
    case SNil => SNil
}


def merge[A](s:Stream[A],t:Stream[A]):Stream[A]= s match{
    case Cons(a, f) => t match {
        case Cons(b,g) => append(toStream(List(a)),(Cons(b,_ => merge(f(),g()))))
        case SNil => SNil
    }
    case SNil => SNil
}

// part three
def all[A](fun: Int => Boolean, s:Stream[A]):Boolean=s match {
    case Cons(a, f) => fun match {
        case fun if fun(a.toString().toInt) => true && all(fun,f())
        case fun if !fun(a.toString().toInt) => false
        case _ => false
    }
    case SNil => false
}

def exists[A](fun: Int => Boolean, s:Stream[A]):Boolean=s match {
    case Cons(a, f) => fun match {
        case fun if fun(a.toString().toInt) => true 
        case fun if !fun(a.toString().toInt) => false || exists(fun,f())
        case _ => true
    }
    case SNil => true
}

// part four
def zipSafe[A](s:Stream[A],t:Stream[A]):Stream[(A,A)] = (s,t) match{
    case (Cons(a,f),Cons(b,g))=> Cons((a,b),_ => zipSafe(f(),g()))
    case (SNil,Cons(a,f)) => Cons((a,a),_ => zipSafe(SNil,f()))  // when one Stream reach to the end, repeat the one left and recursion with SNil as first argument
    case (SNil, SNil) => SNil
}

def mergeSafe[A](s:Stream[A],t:Stream[A]):Stream[A]= (s,t) match{
    case (Cons(a,f),Cons(b,g))=> append(toStream(List(a)),Cons(b,_ => mergeSafe(f(),g())))
    case (SNil,Cons(a,f)) =>Cons(a,_ => mergeSafe(SNil,f()))     // when one Stream reach to the end, only append the redundant one
    case (SNil, SNil) => SNil

}


// the append and toStream methods are from the "Infinite data in Scala" lecture note
def append[A](s: => Stream[A], t: => Stream[A]): Stream[A] = s match {
    case SNil => t
    case Cons(a, f) => Cons(a, _ => append(f(),t))
  }


def toStream[A](l: List[A]): Stream[A] = prepend(l, SNil)

def prepend[A](l: List[A], s: => Stream[A]): Stream[A] = l match {
    case Nil => s
    case (h :: t) => Cons(h, _ => prepend(t, s))
}