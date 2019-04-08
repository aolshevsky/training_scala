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

  // exercise 3.11
  def sumL(l: List[Int]) = {
    foldLeft(l, 0)(_ + _)
  }

  def productL(l: List[Int]) = {
    foldLeft(l, 1)(_ * _)
  }

  // exercise 3.12
  def reverse[A](l: List[A]) = {
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))
  }

  // exercise 3.13 ??

  // exercise 3.14
  def appendByFoldRight[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)(Cons(_, _))
  }

  // exercise 3.15
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(appendByFoldRight)
  }

  // exercise 3.16
  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((x, xs) => Cons(x + 1, xs))
  }

  // exercise 3.17
  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((x, xs) => Cons(x.toString, xs))
  }

  // exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x), xs))
  }

  // exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)
  }

  // exercise 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  // exercise 3.21
  def filterByFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  // exercise 3.22
  def sum_lists(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sum_lists(xs, ys))
  }

  // exercise 3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  // exercise 3.24 ---
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case Cons(_, xs) => hasSubsequence(xs, sub)
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

     // test exercise 3.11
     println(List.sumL(l))
     println(List.productL(l))
 
     // test exercise 3.12
     println(List.reverse(l))
 
     // test exercise 3.14
     println(List.appendByFoldRight(l, l))
 
     // test exercise 3.15
     val lOfL = List(
       List(1, 2, 3), Nil, List(4, 5, 6), List(), List(7, 8)
     )
     println(List.concat(lOfL))

    // test exercise 3.16
    println(List.addOne(l))

    // test exercise 3.17
    println(List.doubleToString(ex4))

    // test exercise 3.18
    println(List.map(l)((i: Int) => i + 1))

    // test exercise 3.19
    println(List.filter(l)((i: Int) => i % 2 == 0))

    // test exercise 3.20
    println(List.flatMap(List(1,2,3))(i => List(i, i)))
    
    // test exercise 3.21
    println(List.filterByFlatMap(l)(isEven))

    // test exercise 3.22
    println(List.sum_lists(l, l)) // 2, 4, 6...

    // test exercise 3.23
    println(List.zipWith(l, l)(_ * _)) // 1, 4, 9...
  }
}