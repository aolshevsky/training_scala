abstract class Option[+A] {

    // exercise 4.1
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }
  
    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }
  
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }
  
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
  
  object Option {
  
    def failingFn(i: Int): Int = {
      try {
        val x = 42 + 5
        x + ((throw new Exception("fail")): Int)
      }
      catch { case e: Exception => 43 }
    }
  
    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }
  
    // exercise 4.2
    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }
  
    // exercise 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(aa => b.map(bb => f(aa, bb)))
    }
  
    def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???
  
    def Try[A](a: => A): Option[A] = {
      try Some(a)
      catch { case e: Exception => None}
    }
  
    def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
      val optAge: Option[Int] = Try {age.toInt}
      val optTickets : Option[Int] = Try {numberOfSpeedingTickets.toInt}
      map2(optAge, optTickets)(insuranceRateQuote)
    }
  
    // exercise 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case None :: _ => None
      case Some(r) :: t => sequence(t) map(r :: _)
    }
  
    def parseInts(a: List[String]): Option[List[Int]] = {
      sequence(a map (i => Try(i.toInt)))
    }
  
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
    }
  
    // exercise 4.5
    def sequenceByTraverse[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(x => x)
    }
  
  
    def main(args: Array[String]): Unit = {
      // test exercise 4.1
      println(failingFn(42))
      val f = (s: String) => s + "-mapped"
      println(Some("some").map(f))
  
      val d = Some("else")
      println(None.getOrElse(d))
      println(Some("some").getOrElse(d))
  
      val f1 = (s: String) => Some(s + "-mapped")
      println(Some("some").flatMap(f1))
  
      // test exercise 4.2
      println(Option.variance(Seq(1.0, 2.0)))
      println(None.flatMap(f1))
  
      // test exercise 4.3
      val f2 = (a: Int, b: Int) => a + b
      println("Some(4) is equal: " + map2(Some(1), Some(3))(f2))
  
      // test exercise 4.4
      println(Option.sequence(List(Some(1), Some(2)))) // Some(List(1, 2)
      println(Option.sequence(List(Some(1), None)))
      println(Option.parseInts(List("12", "14")))
  
      val f3 : String => Option[Int] = (s: String) => Option.Try(s.toInt)
      println(Option.traverse(List("1", "2", "3"))(f3))
  
      // test exercise 4.5
      println(Option.sequenceByTraverse(List(Some(1), Some(2)))) // Some(List(1, 2)
  
    }
  }