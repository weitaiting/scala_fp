package chapter_2

import org.scalatest.flatspec.AnyFlatSpec

class Exercise2_2_Test extends AnyFlatSpec {

  "isSorted" should "return true if the array is sorted according to the comparison function" in {
    assert(Exercise2_2.isSorted(Array(), (x: Int, y: Int) => x < y))
    assert(Exercise2_2.isSorted(Array(0), (x: Int, y: Int) => x < y))
    assert(Exercise2_2.isSorted(Array(-500, 0, 5, 20), (x: Int, y: Int) => x < y))
    assert(!Exercise2_2.isSorted(Array(19, 2, 2, 1), (x: Int, y: Int) => x > y))
    assert(Exercise2_2.isSorted(Array(19, 3, 2, 1), (x: Int, y: Int) => x > y))
    assert(!Exercise2_2.isSorted(Array(1, 3, 2, 20), (x: Int, y: Int) => x < y))
  }

}
