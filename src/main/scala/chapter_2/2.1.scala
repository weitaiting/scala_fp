package chapter_2

object Exercise2_1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def rec(n: Int, x: Int, y: Int): Int = {
      if (n == 0) x
      else rec(n - 1, y, x + y)
    }
    rec(n, 0, 1)
  }
}

