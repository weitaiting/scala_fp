import collection.mutable.Stack
import org.scalatest.flatspec.AnyFlatSpec

class Exercise2_1_Test extends AnyFlatSpec {

  "fib" should "return the n-th fibonacci value" in {
    assert(Exercise2_1.fib(0) == 0)
    assert(Exercise2_1.fib(1) == 1)
    assert(Exercise2_1.fib(2) == 1)
    assert(Exercise2_1.fib(3) == 2)
    assert(Exercise2_1.fib(4) == 3)
    assert(Exercise2_1.fib(5) == 5)
    assert(Exercise2_1.fib(6) == 8)
  }

}
