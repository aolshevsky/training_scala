abstract class Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // exercise 3.29
  def fold[IN, ACC](t: Tree[IN])(f: IN => ACC)(g: (ACC, ACC) => ACC): ACC = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeByFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }

  def maximumByFold(t: Tree[Int]): Int = {
    fold(t)(v => v)(_ max _)
  }

  def depthByFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)(1 + _ max _)
  }

  def mapByFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
  }

  def main(args: Array[String]): Unit = {
    // test exercise 3.25
    val l1 = Leaf(1)
    println(Tree.size(l1)) // 1

    val l2 = Leaf(2)
    val b1 = Branch(l1, l2)
    println(Tree.size(b1)) // 3

    val l3 = Leaf(3)
    val b2 = Branch(b1, l3)
    println(Tree.size(b2)) // 5
    println(b2)

    // test exercise 3.26
    println(Tree.maximum(b2))

    // test exercise 3.27
    println("Depth: " + Tree.depth(b2)) // 2

    val b3 = Branch(b2, l3)
    println("Depth: " + Tree.depth(b3)) // 3

    // test exercise 3.28
    println("Map tree: " + Tree.map(b3)((i: Int) => i * i)) // 1, 4, 9...

    // test exercise 3.29
    println("Size(branches + leafs): " + Tree.sizeByFold(b2)) // 5
    println("Maximum in tree: " + Tree.maximumByFold(b2)) // 3
    println("Depth: " + Tree.depthByFold(b3)) // 3
    println("Map tree: " + Tree.mapByFold(b3)((i: Int) => i + 5)) // 6, 7, 8...
  }
}