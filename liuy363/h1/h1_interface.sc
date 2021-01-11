import $file.h1, h1._
// Fill in your constructors here.

def BT_node[A](l: BinTree[A], a: A, r: BinTree[A]): BinTree[A] = h1.BinBranch(a,l,r)
def BT_leaf[A]: BinTree[A] = h1.Node()
def BT_flatten[A](t: BinTree[A]): List[A] = h1.flatten(t)
def BT_orderedElems(t: BinTree[Int]): List[Int] = h1.orderedElems(t)

def LT_node[A](l : LeafTree[A], r: LeafTree[A]): LeafTree[A] = h1.Branch(l,r)
def LT_leaf[A](a: A): LeafTree[A] = h1.Leaf(a)
def LT_flatten[A](t: LeafTree[A]): List[A] = h1.flatten(t)
def LT_orderedElems(t: LeafTree[Int]): List[Int] = h1.orderedElems(t)

