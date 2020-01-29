import scala.util.Random

object Application extends App {
  type And[A, B]=(A, B)
  type Or[A, B] = Either[A, B]
  type /\[A, B] = (A, B)
  type \/[A, B] = Either[A, B]
  type <==>[A, B] = (A => B) /\ (B => A)
  type Not[A] = A => Nothing

  def and_1[A, B](n: A /\ B): A = n._1
  def and_2[A, B](n: A /\ B): B = n._2
  def or_1[A, B](a: A): A \/ B = Left(a)
  def or_2[A, B](b: B): A \/ B = Right(b)
  def mp[A, B](a: A, f: A => B): B = f(a)
  def exp[A, B, C](a: A, g: (A /\ B) => C): B => C = b => g((a, b))
  def bicond_1[A, B](f: A <==> B): A => B = f._1
  def bicond_2[A, B](f: A <==> B): B => A = f._2

  def ex_falso_1[A](n: Nothing): A = n
  def ex_falso_2[A](n: Nothing): Not[A] = a => n // n, _ => n
  /*def dist_law[A, B, C](h: A \/ (B /\ C)): (A /\ B) \/ (A /\ C) = {
    val a = Left(h)
    val b = Left(Right(h))
    val c = Right(Right(h))
    val left_of_either = (a , b)
    val right_of_either = (a, c)
    Left(left_of_either)
  }*/
  //def dist_law_1[A, B, C](h: A And (B Or C)): (A Or B) And (A Or C) = ((), ())
  // def dist_law_2[A, B, C](h: A And (B Or C)): (A Or B) And (A Or C) = h

  //def shunting[A, B, C](n: A /\ B => C): A => B => C = ???

  // cp-total-exercises




  def parseInt(number: String): Either[NumberFormatException, Int] = // Try(number.toInt)
    try {
      Right(number.toInt)
    } catch {
      case e: NumberFormatException => Left(e)
    }

  // -- sanity check
  parseInt("oops")
  // can restrict input
  def root(number: Int): Either[IllegalArgumentException, Int] = {
    val sqrt = Math.sqrt(number.toDouble)
    if (sqrt.isNaN) Left(new IllegalArgumentException)
    else Right(sqrt.toInt)
  }
  // -- sanity check
  root(-1)

  // can introduce NotEmptyList
  def head[A](list: List[A]): Option[A] = list.headOption
  // -- sanity check
  head(Nil)

  def find[K, V](dictionary: Map[K, V], key: K): Option[V] =
    if (dictionary.contains(key)) Some(dictionary(key))
    else None

  // -- sanity check
  find(Map.empty[Int, String], 1)
  find(Map(1 -> "Uno"), 2)

  def safeSquare(i: Int): Either[IllegalArgumentException, Int] = {
    if (i > 46340) Left(new IllegalArgumentException("Input too high"))
    else if (i < -46340) Left(new IllegalArgumentException("Input too low"))
    else Right(i * i)
  }
  // -- sanity check
  safeSquare(46500)







  val number = -5
  def abs: Int = {
    if (number < 0) -number
    else number
  }
  // -- sanity check
  (number + abs) == (abs + number)

  val total = 0
  def addToTotal(a: Int, total: Int) : Int = {
    total+a
  }
  // -- sanity check
  val before = total * 2
  //addToTotal(3,total)
  before == total * 2

  def getLargestNumber(x: Int, y: Int) : Int = if(x>y) x else y
  // -- sanity check
  getLargestNumber(-1, -2) == -1
  getLargestNumber(1, 2) == 2
  getLargestNumber(-1, -2) == -1

  // -- start external context
  def unknown(x: Int): Int = unknown(x)
  // -- end
  def first[A, B](a: => A, b: => B): A = a
  // -- sanity check
  first(1, unknown(2))

  def randomIntBetween(min: Int, max: Int)(seed: Long) : Int = {
    val r = new Random()
    r.setSeed(seed)
    min + r.nextInt(max - min)
  }
  // -- sanity check
  randomIntBetween(1, 2)(100) == randomIntBetween(1, 2)(100)

  type State[S, A] = S => (S, A)
  def addTotal(a:Int): State[Int, Unit] = total => (total + a, ())


  def sumOfAllNumbers(): Int = {
     Array(10,20,30,40,50).sum
    /*var N:Int=0;
    var sum: Int=0;
    for ( N <- numbers ) {
      sum+=N;
    }
    sum*/
  }
  // -- sanity check
  sumOfAllNumbers() == 150

  def evenElements[A](ls: List[A]): List[A] = {
    var res: List[A] = Nil
    var odd = true
    while (true) {
      if (odd != odd)
        res = ls.head :: res
    }
    res
  }
  // -- sanity check
  evenElements(List(1, 2, 3, 4, 5)) == List(2, 4)

  def isPalindrome[A](ls: List[A]): Boolean = ls match {
    case first +: rest :+ last if first == last => isPalindrome(rest)
    case Nil => false
  }
  // -- sanity check
  isPalindrom(List(1, 2, 1)) == true
  isPalindrom(List(1)) == true
  isPalindrom(List(1, 2, 2, 1)) == true

  def largest[A <% Ordered[A]](ls: List[A]): A = ls match {
    case a :: Nil => a
    case a :: as =>
      val las = largest(as)
      if (a > las) a else las
  }
  // -- sanity check
  largest(List(1, 5, 2)) == 5
  largest(List(5, 1, 2)) == 5
  largest(List(2, 1, 5)) == 5

  // -- get the nth Catalan number
  // (1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862)
  // -- start external context
  object Succ { def unapply(i: Int) = if (i == 0) None else Some(i - 1) }
  // -- end external context
  def catalan(i: Int): Long = i match {
    case 0 => 1
    case 1 => 1
    case Succ(n) => (0 to n).map(i => catalan(i)*catalan(n - i)).sum
  }
  // -- sanity check
  catalan(0) == 1
  catalan(1) == 1
  catalan(3) == 5
  catalan(6) == 132

  // -- start external context
  val data: Iterator[Int] = List(1, 2, 3).iterator // emulate input from environment
  sealed trait Result
  case class Data(data: Int) extends Result
  case object Done extends Result
  case class Error(message: String) extends Result
  def receive: Result = if (data.hasNext) Data(data.next) else Done
  // -- end external context
  def getAll(): Option[List[Int]] = {
    def accumulate(accum: List[Int]): Option[List[Int]] =
      receive match {
        case Data(data) => accumulate(data :: accum)
        case Done => Some(accum.reverse)
        case Error(_) => None
      }

    accumulate(Nil)
  }
  // -- sanity check
  getAll() == List(1, 2, 3)
}
// COQ, AGDA???
// bit.ly/cp-termination-excercises

// cp-logic-slides
// constructive.dev -> subset of functional programming with more restriction
// prove termination via type system - ramsey theorem
// Indris language
// cp-logic-answers
// cp-logic-exercises