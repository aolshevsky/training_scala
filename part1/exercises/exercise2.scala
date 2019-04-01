object Fibonacci {

  def fibonacci(n: Int): Int = {
      if (n <= 0)
        throw new IllegalArgumentException("Input number should be larger than zero.")
      
      if (n < 2) n
      else fibonacci(n - 2) + fibonacci(n - 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit =
    println("Hello, world!")
}

object PolymorphicFunctions {

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(i: Int): Int = 
      if (i >= as.length) -1
      else if (p(as(i))) i
      else loop(i + 1)

    loop(0)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(i: Int): Boolean =
      if (i >= as.length - 1) true
      else if (ordered(as(i), as(i + 1))) loop(i + 1)
      else false

    loop(0)
  }

  // exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = 
    (a: A) => (b: B) => f(a, b)
  
  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
  
  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}