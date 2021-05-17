package chapter_2

object Exercise2 {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def rec(n: Int, x: Int, y: Int): Int = {
      if (n == 0) x
      else rec(n - 1, y, x + y)
    }
    rec(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def rec(
        as: Array[A],
        ordered: (A, A) => Boolean,
        position: Int
    ): Boolean = {
      if (as.isEmpty) true
      else if (as.length == 1) true
      else if (position + 1 == as.length) true
      else if (!ordered(as(position), as(position + 1))) false
      else rec(as, ordered, position + 1)
    }
    rec(as, ordered, 0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (input: A) => ((input2: B) => f(input, input2))
    // alternative concise solution with syntactic sugar
    // (input: A) => f(input, _)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (input: A, input2: B) =>
    f(input)(input2)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = { (input: A) =>
    f(g(input))
  }

}
