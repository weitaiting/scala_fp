package misc

trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def mapLeaves[A, B](root: Tree[A])(f: A => B): Tree[B] = {
    case class Frame(
        done: List[Tree[B]],
        todos: List[Tree[A]]
    )

    @annotation.tailrec
    def step(stack: List[Frame]): Tree[B] = {
      System.out.println(stack)
      // "return / pop a stack-frame"
      val frame :: frames = stack
      frame match {
        case Frame(done, Nil) => {
          if (done.size != 2) {
            throw new Error("should not happen, not 2")
          }
          val ret = Branch(done(1), done(0))
          frames match {
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
        case Frame(done, x :: xs) => {
          x match {
            // "recursion base"
            case Leaf(v) => step(Frame(Leaf(f(v)) :: done, xs) :: frames)
            // "recursive call"
            case Branch(left, right) =>
              step(Frame(Nil, List(left, right)) :: Frame(done, xs) :: frames)
          }
        }
      }
    }

    root match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) =>
        step(List(Frame(Nil, List(left, right))))
    }
  }
}
