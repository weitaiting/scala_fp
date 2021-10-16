package chapter_3

import org.scalatest.flatspec.AnyFlatSpec

class Exercise3ListTest extends AnyFlatSpec {
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

  "apply" should "allow us to generate a list by specifying each list element as an arg" in {
    assert(List() == Nil)
    assert(List(1) == Cons(1, Nil))
    assert(List(1, 2) == Cons(1, Cons(2, Nil)))
  }

  "tail" should "remove the first element of a list" in {
    assert(List.tail(List()) == Nil)
    assert(List.tail(List(5, 4, 3)) == List(4, 3))
  }

  "setHead" should "change the first element of a list" in {
    assert(List.setHead(List(), 3) == List())
    assert(List.setHead(List(1, 2, 3), 3) == List(3, 2, 3))
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

  "reverse" should "return a list with elements reversed" in {
    assert(List.reverse(Nil) == Nil)
    assert(List.reverse(List("a", "b")) == List("b", "a"))
    assert(List.reverse(List(1, 3, 5)) == List(5, 3, 1))
  }

  "init" should "remove the last element of a list" in {
    assert(List.init(List(1, 3, 5)) == List(1, 3))
    assert(List.init(List(1)) == Nil)
    assert(List.init(Nil) == Nil)
  }

  "init2" should "remove the last element of a list" in {
    assert(List.init2(List(1, 3, 5)) == List(1, 3))
    assert(List.init2(List(1)) == Nil)
    assert(List.init2(Nil) == Nil)
  }

  "sum2" should "return the sum of a list of ints" in {
    assert(List.sum2(List()) == 0)
    assert(List.sum2(List(1, 2, 3, 4, 5)) == 15)
    assert(List.sum2(List(-5, 2, 0)) == -3)
  }

  "product2" should "return the product of a list of doubles" in {
    assert(List.product2(List()) == 1.0)
    assert(List.product2(List(1, 3, 1.5)) == 4.5)
    assert(List.product2(List(2, -3.0)) == -6.0)
    assert(List.product2(List(5, 3, 0.0)) == 0.0)
  }

  "length" should "return the length of a list" in {
    assert(List.length(Nil) == 0)
    assert(List.length(List(1, 2, 3)) == 3)
  }

  "sum3" should "return the sum of a list of ints" in {
    assert(List.sum3(List()) == 0)
    assert(List.sum3(List(1, 2, 3, 4, 5)) == 15)
    assert(List.sum3(List(-5, 2, 0)) == -3)
  }

  "product3" should "return the product of a list of doubles" in {
    assert(List.product3(List()) == 1.0)
    assert(List.product3(List(1, 3, 1.5)) == 4.5)
    assert(List.product3(List(2, -3.0)) == -6.0)
    assert(List.product3(List(5, 3, 0.0)) == 0.0)
  }

  "length2" should "return the length of a list" in {
    assert(List.length2(Nil) == 0)
    assert(List.length2(List(1, 2, 3)) == 3)
  }

  "reverse2" should "return a list with elements reversed" in {
    assert(List.reverse2(Nil) == Nil)
    assert(List.reverse2(List("a", "b")) == List("b", "a"))
    assert(List.reverse2(List(1, 3, 5)) == List(5, 3, 1))
  }

  "append" should "append an element to the list" in {
    assert(List.append(List())("a") == List("a"))
    assert(List.append(List(1, 3))(5) == List(1, 3, 5))
  }

  "prependAll" should "prepend every item in the target list to the accumulator" in {
    assert(List.prependAll(List(1, 3))(List(2, 4)) == List(2, 4, 1, 3))
  }

  "flatten" should "flatten a list of lists" in {
    assert(List.flatten(List(List(1, 2), List(3), List(), List(4, 5))) == List(1, 2, 3, 4, 5))
  }

  "addOne" should "add 1 to each int in a list of ints" in {
    assert(List.addOne(List(1, 2, 3)) == List(2, 3, 4))
  }

  "doubleToString" should "turn each val in the list to a string" in {
    assert(List.doubleToString(List(2.0, 3.0)) == List("2.0", "3.0"))
  }

  "map" should "modify each element in a list with an arbitrary fn" in {
    assert(List.map(List[Int]())(x => x * 2) == List())
    assert(List.map(List(2, 3))(x => x * 2) == List(4, 6))
  }

  "filter" should "remove elements from a list that do not satisfy a fn" in {
    assert(List.filter(List(1, 2, 3, 4))(x => x % 2 == 1) == List(1, 3))
  }

  "filterOutOddNumbers" should "remove odd numbers from a list" in {
    assert(List.filterOutOddNumbers(List(1, 2, 3, 4)) == List(2, 4))
  }

  "flatMap" should "map then flatten" in {
    assert(List.flatMap(List(1, 2, 3))(x => List(x * 1, x * 2)) == List(1, 2, 2, 4, 3, 6))
  }

  "filter2" should "remove elements from a list that do not satisfy a fn" in {
    assert(List.filter2(List(1, 2, 3, 4))(x => x % 2 == 1) == List(1, 3))
  }

  "sumTwoLists" should "sum two lists" in {
    assert(List.sumTwoLists(List(1, 2, 3), List(1, 0, -1)) == List(2, 2, 2))
  }

  "zipWith" should "apply a fn to each element paired across 2 lists" in {
    assert(List.zipWith(List(1, 2, 3), List(1, 0, -1), (x: Int, y: Int) => x * y) == List(1, 0, -3))
  }
}

class Exercise3TreeTest extends AnyFlatSpec {
  "size" should "return the number of leaf and branch nodes on the tree" in {
    assert(Tree.size(Leaf(5)) == 1)
    assert(Tree.size(Branch(Leaf(1), Leaf(0))) == 3)
    assert(Tree.size(Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))) == 5)
  }

  "maximum" should "return the max element in the tree" in {
    assert(Tree.maximum(Leaf(5)) == 5)
    assert(Tree.maximum(Branch(Leaf(1), Leaf(0))) == 1)
    assert(Tree.maximum(Branch(Branch(Leaf(2), Leaf(-3)), Branch(Leaf(6), Leaf(0)))) == 6)
  }

  "depth" should "return the max depth of a tree" in {
    assert(Tree.depth(Leaf(5)) == 0)
    assert(Tree.depth(Branch(Leaf(1), Leaf(2))) == 1)
    assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 2)
    assert(Tree.depth(Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Leaf(2))))) == 3)
    assert(Tree.depth(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))) == 3)
  }

  "map" should "modify each element of a tree according to the given fn" in {
    assert(Tree.map[Int, Int](Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(x => x) == Branch(Leaf(1), Leaf(2)))
  }
}