package misc

import org.scalatest.flatspec.AnyFlatSpec

class MiscTest extends AnyFlatSpec {
  "mapLeaves" should "map the leaves of the Tree" in {
    val example = Branch(
      Branch(
        Leaf("a"),
        Leaf("b")
      ),
      Branch(
        Leaf("c"),
        Branch(
          Leaf("d"),
          Leaf("e")
        )
      )
    )

    val expected = Branch(
      Branch(
        Leaf("A"),
        Leaf("B")
      ),
      Branch(
        Leaf("C"),
        Branch(
          Leaf("D"),
          Leaf("E")
        )
      )
    )
    val result =
      Tree.mapLeaves(example, { case Leaf(n) => Leaf(n.toUpperCase) })
    assert(result == expected)
  }
}
