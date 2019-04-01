abstract class List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("Cannot get tail from an empty list.")
    case Cons(_, xs) => xs
  }

  // exercise 3.3
  def setHead[A](l: List[A], v: A): List[A] = l match {
    case Nil => throw new NoSuchElementException("Cannot get head from an empty list.")
    case Cons(_, xs) => Cons(v, xs)
  }

  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
  }

  // exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("No initial element for an empty list.")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // exercise 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, b) => b + 1)
  }


  // exercise 3.10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def main(args: Array[String]): Unit = {
    val ex1 : List[Double] = Nil
    val ex2: List[Int] = List(1,2,3,4,5)
    val ex3 : List[String] = Cons("a", Cons("b", Nil))
    println(ex1)
    println(ex2)
    println(ex3)
    println("Sum: " + sum(ex2))
    val ex4 : List[Double] = Cons(1.2, Cons(2, Nil))
    println("Product: " + product(ex4))

    val ex2_tail = tail(ex2)
    val ex2_head = setHead(ex2, 33)
    println(ex2_tail)
    println(ex2_head)

    val ex2_drop = drop(ex2, 2)
    println(ex2_drop)

    val func1 = (v: Int) => v < 4
    val ex2_dropWhile = dropWhile(ex2, func1)
    println(ex2_dropWhile)

    val ex2_init = init(ex2)
    println(ex2_init)

    // test exercise 3.8
    val ex5 = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    println(ex5)

    val ex2_length = length(ex2)
    println(ex2_length) // 5

    val l = List(1, 2, 3, 4, 5)
    println(List.foldLeft(l, 10)(_ + _)) // 25
  }
}