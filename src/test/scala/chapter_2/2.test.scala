package chapter_2

import org.scalatest.flatspec.AnyFlatSpec

class Exercise2_Test extends AnyFlatSpec {

  "fib" should "return the n-th fibonacci value" in {
    assert(Exercise2.fib(0) == 0)
    assert(Exercise2.fib(1) == 1)
    assert(Exercise2.fib(2) == 1)
    assert(Exercise2.fib(3) == 2)
    assert(Exercise2.fib(4) == 3)
    assert(Exercise2.fib(5) == 5)
    assert(Exercise2.fib(6) == 8)
  }

  "isSorted" should "return true if the array is sorted according to the comparison function" in {
    assert(Exercise2.isSorted(Array(), (x: Int, y: Int) => x < y))
    assert(Exercise2.isSorted(Array(0), (x: Int, y: Int) => x < y))
    assert(Exercise2.isSorted(Array(-500, 0, 5, 20), (x: Int, y: Int) => x < y))
    assert(!Exercise2.isSorted(Array(19, 2, 2, 1), (x: Int, y: Int) => x > y))
    assert(Exercise2.isSorted(Array(19, 3, 2, 1), (x: Int, y: Int) => x > y))
    assert(!Exercise2.isSorted(Array(1, 3, 2, 20), (x: Int, y: Int) => x < y))
  }

  "curry" should "turn a function (A, B) => C to A => B => C" in {
    val sumFn: (Int, Int) => Int = (x: Int, y: Int) => x + y
    val arbitraryFn: (String, Int) => String = (x: String, y: Int) => x + y.toString()
    assert(Exercise2.curry(sumFn)(3)(5) == 8)
    assert(Exercise2.curry(arbitraryFn)("a")(9) == "a9")
  }

  "uncurry" should "turn a function A => B => C into (A, B) => C" in {
    val curriedSumFn: Int => (Int => Int) = (x: Int) => (y: Int) => x + y
    val curriedArbitraryFn: (String) => (Int => String) = (x: String) => (y: Int) => x + y.toString()
    assert(Exercise2.uncurry(curriedSumFn)(3, 5) == 8)
    assert(Exercise2.uncurry(curriedArbitraryFn)("a", 9) == "a9")
  }

  "compose" should "compose two functions" in {
    assert(Exercise2.compose((x: Int) => x + 3, (x: Int) => x + 5)(0) == 8)
    assert(Exercise2.compose((x: Int) => x - 5, (x: Int) => x * 2)(1) == -3)
  }

}
