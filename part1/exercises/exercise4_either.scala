abstract class Either[+E, +A] {
    // exercise 4.6
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(r) => Right(f(r))
      case Left(l) => Left(l)
    }
  
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(r) => f(r)
      case Left(l) => Left(l)
    }
  
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(r) => Right(r)
      case Left(_) => b
    }
  
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        a <- this
        bb <- b
      } yield f(a, bb)
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
  
  object Either {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)
    }
  
    def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
      try Right(x / y)
      catch { case e: Exception => Left(e)}
    }
  
    def Try[A](a: => A): Either[Exception, A] = {
      try Right(a)
      catch { case e: Exception => Left(e)}
    }
  
    // test exercise 4.7
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
      case Nil => Right(Nil)
      case x :: xs => x.flatMap(xx => sequence(xs).map(xx :: _))
    }
  
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case x :: xs => (f(x)map2 traverse(xs)(f))(_ :: _)
    }
  
    def main(args: Array[String]): Unit = {
      println(Left("e"))
      println(Right("e"))
  
      println("\nTest map:")
      val f = (s: String) => s + "-mapped"
      Left("e").map(f)
      println(Right("v").map(f))
  
      println("\nTest flatMap:")
      val f1 = (s: String) => Right(s + "-mapped")
      println(Left("e").flatMap(f1))
      println(Right("e1").flatMap(f1))
  
      println("\nTest orElse:")
      val b = Right("else")
      println(Left("e").orElse(b))
      println(Right("v").orElse(b))
  
      println("\nMean" + Either.mean(IndexedSeq(1.0, 2.0, 3.0)))
  
      println("\nDiv 1/1 = " + Either.safeDiv(1, 1))
      println("Div 2/0 = " + Either.safeDiv(2, 0))
  
      println("Try div: " + Either.Try(2/0))
  
      println("\nTest map2:")
      val b1 = Right("else")
      val bLeft = Left("else")
      val f3 = (a: String, b: String) => a + "&" + b
      Left("e").map2(b1)(f3)
      Left("e").map2(bLeft)(f3)
      Right("v").map2(b1)(f3)
      Right("v").map2(bLeft)(f3)
  
      println("\nTest sequence:" + Either.sequence(List(Right(1), Right(2), Right(3), Right(4))))
  
      val f4: Int => Either[String, Int] = (i: Int) => i match {
        case 0 => Left("Zero")
        case p => Right(p)
      }
      println(Either.traverse(List(1))(f4)) // Left(1)
      println(Either.traverse(List(1, 0))(f4)) // Left(Zero)
  
    }
  }