sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// companion object
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  // Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  // Exercise 3.11
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def length3[A](as: List[A]): Int = foldLeft(as, 0)((len, _) => len + 1)

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil:List[A])((h, t) => Cons(t, h))
  }

  // Exercise 3.13
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  // Exercise 3.14
  def append[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)((a, b) => Cons(a, b))
  }
  def append2[A](l: List[A], r: List[A]): List[A] = {
    foldLeft(reverse(r), l)((b, a) => Cons(a, b))
  }

  // variance function
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("set head of empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => sys.error("drop on empty list")
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, xs) if f(h) => dropWhile(xs, f)
    case _ => l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, len) => len + 1)
}

// Exercise 3.1
val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

// Exercise 3.2
List.tail(List(1, 2, 3))
List.tail(List(1))

// Exercise 3.3
List.setHead(3, List(1, 2, 3))
List.setHead("c", List("a", "b"))

List.drop(List(1, 2, 3), 1)
List.drop(List(1, 2, 3), 0)
List.drop(List("a", "b"), 2)
//List.drop(List(1, 2), 3)
//List.drop(Nil, 1)

List.dropWhile(List(1, 2, 3), (x: Int) => x < 2)
List.dropWhile(List(1, 2, 3), (x: Int) => x > 2)
List.dropWhile(List(1, 2, 3), (x: Int) => x > 0)
List.dropWhile(Nil, (x: Int) => x > 0)

List.init(List(1, 2, 3))
List.init(List(1))

// Exercise 3.8
List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))

List.length(List(1, 2, 3, 4, 5))
List.length3(List(1, 2, 3, 4, 5))

List.reverse(List(1, 2, 3))

List.append(List(1, 2, 3), List(1, 2))
List.append(List(1, 2, 3), Nil)
List.append(Nil, List(1, 2))
List.append(Nil, Nil)

List.append2(List(1, 2, 3), List(1, 2))
List.append2(List(1, 2, 3), Nil)
List.append2(Nil, List(1, 2))
List.append2(Nil, Nil)