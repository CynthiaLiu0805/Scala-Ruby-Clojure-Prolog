sealed trait Stream[+A]
case object SNil extends Stream[Nothing]
case class Cons[A](a: A, f: Unit => Stream[A]) extends Stream[A]

//part one
def filter[A](fun: A => Boolean, s:Stream[A]):Stream[A]=s match {
    case Cons(a, f) => fun match {
        case fun if fun(a) => Cons(a,_ => filter(fun,f()))
        //append(toStream(List(a)),filter(fun,f()))
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
        case Cons(b,g) => append(toStream(List(a)),Cons(b,_ => merge(f(),g())))
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






def take[A](n: Int, s: => Stream[A]): List[A] = s match {
  case SNil => Nil
  case Cons(a,f) => n match {
    case n if n > 0 => a :: take(n-1,f())
    case _ => Nil
    }
}

// def drop[A](n:Int, s: => Stream[A]): List[A] = s match {
//   case SNil => Nil
//   case Cons(a,f)=> n match {
//     case n if n > 0 => a :: take(n-1,f())
//     case _ => Nil
//     }
    
// }

// def appendPairs[A](s: => (Stream[A],Stream[A]), t: => (Stream[A],Stream[A])): Stream[A] = s match {
//     case (SNil,SNil) => t
//     case (Cons(a, f),Cons(b, g)) => Cons(a, _ => append(f(),t))
//   }


def append[A](s: => Stream[A], t: => Stream[A]): Stream[A] = s match {
    case SNil => t
    case Cons(a, f) => Cons(a, _ => append(f(),t))
  }

def prepend[A](l: List[A], s: => Stream[A]): Stream[A] = l match {
    case Nil => s
    case (h :: t) => Cons(h, _ => prepend(t, s))
  }

def toStream[A](l: List[A]): Stream[A] = prepend(l, SNil)

def repeat[A](l: List[A]): Stream[A] = prepend(l, repeat(l))





def intsFrom(n: Int): Stream[Int] = Cons(n, _ => intsFrom(n+1))
val nats = intsFrom(0)


def testTake[A](n: Int, s: => Stream[A]): List[A] = s match {
  case SNil => Nil
  case Cons(a,f) => n match {
    case n if n > 0 => a :: testTake(n-1,f())
    case _ => Nil
    }
  }

val evenNats = filter((x: Int) => x % 2 == 0, nats)
val natsGtr100 = filter((x: Int) => x > 100, nats)
val t1=take(10,evenNats)
val t2=take(20,natsGtr100)
val l1=List(1,2,3)
val l2=List(4,5,6,7)
val s1=toStream(l1)
val s2=toStream(l2)
// val z1=z(s1,s2)

def z[A](s:Stream[A],t:Stream[A]):Stream[(A,A)] = (s,t) match{

    case (Cons(a,f),Cons(b,g))=> Cons((a,b),_ => zip(f(),g()))
    case (SNil,Cons(a,f)) => Cons((a,a),_ => zip(SNil,f()))
    case (SNil, SNil) => SNil
}

def m[A](s:Stream[A],t:Stream[A]):Stream[A]= (s,t) match{

    case (Cons(a,f),Cons(b,g))=> append(toStream(List(a)),Cons(b,_ => merge(f(),g())))
    case (SNil,Cons(a,f)) =>Cons(a,_ => merge(SNil,f()))
    case (SNil, SNil) => SNil

}