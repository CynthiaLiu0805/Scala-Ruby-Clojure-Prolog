def isPrime(x:Int): Boolean = {
    if (x==2) {
        return true
    }
    else {
        for (i<-2 until (x-1)){
            if (x%i==0) {
                return false

            }
        }
        return true
    }
}

def isPalindrome[A](L:List[A]): Boolean = L match{
    case Nil => true
    case List(a) => true
    case list => if (L==L.reverse) true else false
}

var l = List()
def digitList(x:Int):List[Int] = {
    return x.toString().map(_.asDigit).toList.reverse      
}

def primePalindrome(x:Int): Boolean = {
    return isPrime(x)&&isPalindrome(digitList(x))
}