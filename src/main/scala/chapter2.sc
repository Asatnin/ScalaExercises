def cur(f: Int => Int)(x: Int) = {
  f(x) + 2
}

cur(x => x * x)(10)

// Exercise 2.1
def fib(n: Int): Int = {
  @annotation.tailrec
  def loop(i: Int, prev: Int, cur: Int): Int = {
    if (i == n) cur
    else loop(i + 1, cur, prev + cur)
  }
  loop(1, 0, 1)
}

fib(8)

// Exercise 2.2
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) = {
  @annotation.tailrec
  def loop(i: Int): Boolean = {
    if (i >= as.length - 1) true
    else if (!ordered(as(i), as(i + 1))) false
    else loop(i + 1)
  }
  loop(0)
}

isSorted(Array(1, 3, 5, 7), (x: Int, y:Int) => x < y)
isSorted(Array(7, 5, 1, 3), (x: Int, y:Int) => x > y)
isSorted(Array("Scala", "Exercises"), (x: String, y: String) => x.length > y.length)


//Exercise 2.3
def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  a => b => f(a, b)
}

/*def f(a: Int, b: Int): Int = a + b
def g(a: Int)(b: Int): Int = a + b

curry(f)(1)(1) == f(1, 1)
curry(f)(1)(1) == g(1)(1)*/

//Exercise 2.4
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}

/*def f(a: Int, b: Int): Int = a + b
def g(a: Int)(b: Int): Int = a + b

uncurry(g)(1, 1) == g(1)(1)
uncurry(g)(1, 1) == f(1, 1)*/

// Exercise 2.5
def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}

def f(b: Int): Int = b / 2
def g(a: Int): Int = a + 2

compose(f, g)(0)
compose(g, f)(0)
compose(f, g)(2)
compose(g, f)(2)
