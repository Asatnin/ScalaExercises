case class Book(title: String, authors: List[String])
val books: List[Book] = List(
  Book(title = "Structure and Interpretation of Computer Programs",
authors = List("Abelson, Harald", "Sussman, Gerald J.")),
Book(title = "Introduction to Functional Programming",
authors = List("Bird, Richard", "Wadler, Phil")),
Book(title = "Effective Java",
authors = List("Bloch, Joshua")),
Book(title = "Java Puzzlers",
authors = List("Bloch, Joshua", "Gafter, Neal")),
Book(title = "Programming in Scala",
authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

for (b <- books; a <- b.authors if a startsWith "Bird,")
  yield b.title

books flatMap (b => b.authors.withFilter(a => a startsWith "Bird,").map(a => b.title))

/*for {
  x <- 2 to 10
  y <- 2 to x
  if x % y == 0
} yield (x, y)

(2 to 10) map (x => (2 to x) withFilter (y => x % y == 0) map (y => (x, y)))
(2 to 10) flatMap (x => (2 to x) withFilter (y => x % y == 0) map (y => (x, y)))
*/
/*val s: String = "abc"
s flatMap (c => "fff")

val l1: List[String] = List("abc")
val l2: List[String] = List("def")
l1 ++ l2*/

/*val w: String = "abacabAdabAcaba"
w.groupBy(c => c.toLower).toList map (pair => (pair._1, pair._2.length)) sortWith((x, y) => x._1 < y._1)

val l = List("aba", "caba", "daba", "caba")
l.foldLeft("")(_ + _)

type Occurrences = List[(Char, Int)]
val whole = List(('a', 2), ('b', 2))
whole.foldLeft(List[Occurrences](Nil)) {
  case (acc, (ch, freq)) => {
    acc ++ ( for { comb <- acc; n <- 1 to freq } yield (ch, n) :: comb )
  }
}

whole.foldRight(List[Occurrences](Nil)) {
  case ((ch, freq), acc) => {
    acc ++ ( for { comb <- acc; n <- 1 to freq } yield (ch, n) :: comb )
  }
}*/