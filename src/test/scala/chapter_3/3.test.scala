package chapter_3

import org.scalatest.flatspec.AnyFlatSpec

class Exercise3Test extends AnyFlatSpec {
  "apply" should "allow us to generate a list by specifying each list element as an arg" in {
    assert(List() == Nil)
    assert(List(1) == Cons(1, Nil))
    assert(List(1, 2) == Cons(1, Cons(2, Nil)))
  }

  "sum" should "return the sum of a list of ints" in {
    assert(List.sum(List()) == 0)
    assert(List.sum(List(1, 2, 3, 4, 5)) == 15)
    assert(List.sum(List(-5, 2, 0)) == -3)
  }

  "product" should "return the product of a list of doubles" in {
    assert(List.product(List()) == 1.0)
    assert(List.product(List(1, 3, 1.5)) == 4.5)
    assert(List.product(List(2, -3.0)) == -6.0)
    assert(List.product(List(5, 3, 0.0)) == 0.0)
  }

  "tail" should "remove the first element of a list" in {
    assert(List.tail(List()) == Nil)
    assert(List.tail(List(5, 4, 3)) == List(4, 3))
  }

  "drop" should "remove the first n elements of a list" in {
    assert(List.drop(List(), 5) == Nil)
    assert(List.drop(List(1, 3, 5), 0) == List(1, 3, 5))
    assert(List.drop(List(5, 4, 3, 2, 1), 6) == Nil)
    assert(List.drop(List(5, 4, 3, 2, 1), 3) == List(2, 1))
  }

  "dropWhile" should "remove the front elements of a list until an element is found that violates the given predicate" in {
    assert(List.dropWhile(List())((x: Int) => x % 2 == 1) == Nil)
    assert(List.dropWhile(List(1, 3, 5))(x => x % 2 == 1) == Nil)
    assert(List.dropWhile(List(1, 3, 5, 2, 4, 6))(x => x % 2 == 1) == List(2, 4, 6))
    assert(List.dropWhile(List(1, 2, 3, 4, 5))(x => x % 2 == 1) == List(2, 3, 4, 5))
  }

  "setHead" should "change the first element of a list" in {
    assert(List.setHead(List(), 3) == List())
    assert(List.setHead(List(1, 2, 3), 3) == List(3, 2, 3))
  }

  "init" should "remove the last element of a list" in {
    assert(List.init(List(1, 2, 3)) == List(1, 2))
  }

  "length" should "return the length of a list" in {
    assert(List.length(Nil) == 0)
    assert(List.length(List(1, 2, 3)) == 3)
  }

}