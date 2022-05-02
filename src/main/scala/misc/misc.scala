package misc

trait Tree {}
case class Branch(left: Tree, right: Tree) extends Tree
case class Leaf(name: String) extends Tree

object Tree {
  def mapLeaves(root: Tree, f: Leaf => Leaf): Tree = {
    case class Frame(
        done: List[Tree],
        todos: List[Tree]
    )

    @annotation.tailrec
    def step(stack: List[Frame]): Tree = {
      System.out.println(stack)
      stack match {
        // "return / pop a stack-frame"
        case Frame(done, Nil) :: tail => {
          if (done.size != 2) {
            throw new Error("should not happen, not 2")
          }
          val ret = Branch(done(1), done(0))
          tail match {
            case Nil => ret
            case Frame(td, tt) :: more => {
              if (td.size != 0) {
                System.out.println("tasks done not empty")
                System.out.println(td)
              }
              step(Frame(ret :: td, tt) :: more)
            }
          }
        }
        case Frame(done, x :: xs) :: tail => {
          x match {
            // "recursion base"
            case l @ Leaf(_) => step(Frame(f(l) :: done, xs) :: tail)
            // "recursive call"
            case Branch(left, right) =>
              step(Frame(Nil, List(left, right)) :: Frame(done, xs) :: tail)
          }
        }
        case Nil => throw new Error("shouldn't happen")
      }

    }

    root match {
      case l @ Leaf(_) => f(l)
      case Branch(left, right) =>
        step(List(Frame(Nil, List(left, right))))
    }
  }
}
