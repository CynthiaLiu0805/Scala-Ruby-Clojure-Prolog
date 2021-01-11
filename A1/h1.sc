

def hello(i:Int){
    println("hello" + "!"*i)
}
hello(2)

//leaf tree
sealed trait LeafTree[+A]
//case class Empty[A]() extends LeafTree[A]
case class Leaf[A](value: A) extends LeafTree[A]
case class Branch[A](left: LeafTree[A], right: LeafTree[A]) extends LeafTree[A]

//val x1 = Empty[Int]()
//val x2 = Empty[Int]()

//bin tree
sealed trait BinTree[+A]
//case class Node[A](value:A) extends BinTree[A]
case class Node[A]() extends BinTree[A]
case class BinBranch[A](data: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]

val bb1=BinBranch(2,Node(),Node())
val bb2=BinBranch(4,Node(),Node())
val bb3=BinBranch(5,Node(),Node())
val bb4=BinBranch(3,bb2,bb3)
val bb5=BinBranch(1,bb1,bb4)



//case class Node[A](data: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]
//val bt_empty = Node

val lt_1 = Leaf(1)
val lt_231 = Branch(Leaf(2), Branch(Leaf(3), Leaf(1)))



//flatten for leaf tree
def flatten[A](tree:LeafTree[A]):List[A]=tree match{
	//println([a,s,f,])
	case Leaf(value) => List(value)
	case Branch(left,right) => flatten(left):::flatten(right)
}

//flatten for bin tree
def flatten[A](tree:BinTree[A]): List[A]=tree match {
	case Node() => List()
	//case Node => List[Any]()
	case BinBranch(data,left,right) => flatten(left):::List(data):::flatten(right)
}

//sorting algorithmz
def mySort(l: List[Int]):List[Int]={
	val Arr: Array[Int]=l.toArray
	for( i <- 0 until (Arr.length)){
		var min=Arr(i)
		for( j <- i+1 until Arr.length){
			if(Arr(i) > Arr(j)){
				min=Arr(j)
				var temp = Arr(i)
				Arr(i) = Arr(j)
				Arr(j) = temp
			}
//			if (min!=l(i)){
//				val temp= l(i)
//				l(i) = min
//				l(j) = l(i)
//			}
		}
	}
	return Arr.toList
}

//order the list for leaf tree
def orderedElems(tree:LeafTree[Int]): List[Int]=tree match{
	case _ => mySort(flatten(tree).map(_.toString.toInt))
}

//order the list for bin tree
def orderedElems(tree:BinTree[Int]): List[Int]=tree match{
	case _=> mySort(flatten(tree).map(_.toString.toInt))
}

//struct tree
sealed trait StructTree[+A,+B]
case class StructLeaf[A,B](value:B) extends StructTree[A,B]
case class StructBranch[A,B](data: A, left: StructTree[A,B], right:StructTree[A,B]) extends StructTree[A,B]

val sl1=StructLeaf(10)
val sl2=StructLeaf(20)
val sl3=StructLeaf(30)
val sl4=StructLeaf(40)
val sb1=StructBranch("plus",sl1,sl2)
val sb2=StructBranch("minus",sl3,sl4)
val sb3=StructBranch("times",sb1,sb2)



//flatten for struct tree
def flatten[A](tree:StructTree[A,A]): List[A]=tree match {
	case StructLeaf(value) => List(value)
	case StructBranch(data, left, right) => flatten(left) ::: List(data) ::: flatten(right)
}
