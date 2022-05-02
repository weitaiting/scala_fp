package chapter_3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object List {
  def isEmpty[A](ls: List[A]): Boolean = ls match {
    case Nil => true
    case _   => false
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Ex 3.2: Returns a list with the first element removed
  def tail[A](items: List[A]): List[A] = items match {
    case Nil         => Nil
    case Cons(_, xs) => xs
  }

  // Ex 3.3: Replace the first element of a list with a different value
  def setHead[A](items: List[A], sub: A): List[A] = items match {
    case Nil         => Nil
    case Cons(_, xs) => Cons(sub, xs)
  }

  // Ex 3.4: Removes the first n elements from a list
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  // Ex 3.5: Removes elements from the list as long as they match a predicate
  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs)(f)
      else l
  }

  @annotation.tailrec
  def reverse[A](l: List[A], acc: List[A] = Nil): List[A] = l match {
    case Nil         => acc
    case Cons(x, xs) => reverse(xs, Cons(x, acc))
  }

  // a tail-recursive solution
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def tailRec(l: List[A], acc: List[A]): List[A] = l match {
      case Nil          => Nil
      case Cons(_, Nil) => reverse(acc)
      case Cons(x, xs)  => tailRec(xs, Cons(x, acc))
    }
    tailRec(l, Nil)
  }

  // Ex 3.6: Remove the last element of a List
  // a non-tail-recursive solution
  def init2[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init2(xs))
  }

  // NOT tail-recursive, and hence not stack-safe
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)((x, y) => x * y)

  // product using foldRight can't immediately halt the recursion and return 0.0 if it encounters it
  // because it still needs to pop off the frames pushed onto its call stack before it

  // Ex 3.9 Compute length of a list using foldRight
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  // Ex 3.10 Write a tail-recursive foldLeft
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // Ex 3.11 Write tail-recursive sum, product, and length using foldLeft
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)((x, y) => x * y)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  // Ex 3.12 Write a reverse implemented with foldLeft
  def reverse2[A](ns: List[A]) =
    foldLeft(ns, List[A]())((x, y) => Cons(y, x))

  // Ex 3.13 Write foldLeft in terms of foldRight
  // and write foldRight in terms of foldLeft
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((x, y) => f(y, x))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((x, y) => f(y, x))

  // Ex 3.14 Implement append in terms of foldLeft
  // This function appends element z to the list as
  def append[A](as: List[A])(z: A): List[A] =
    foldLeft(reverse(as), List(z))((x, y) => Cons(y, x))

  // Ex 3.15 Write a fn that concatenates a list of lists into a single list
  // with runtime linear in the total length of all lists

  // prependAll prepends every item in the target list to the accumulator
  def prependAll[A](acc: List[A])(target: List[A]): List[A] =
    foldLeft(reverse(target), acc)((x, y) => Cons(y, x))

  // Flatten concatenates a list of lists into a single list
  def flatten[A](as: List[List[A]]): List[A] =
    foldLeft(as, List[A]())((x, y) => prependAll(y)(x))

  // Ex 3.16: Write a fn that adds 1 to each int in a list of ints
  def addOne(as: List[Int]): List[Int] =
    reverse(
      foldLeft(as, List[Int]())((x, y) => Cons(y + 1, x))
    )

  // Ex 3.17: Write a fn that turns each val in a List[Double] to a string
  def doubleToString(as: List[Double]): List[String] =
    reverse(
      foldLeft(as, List[String]())((x, y) => Cons(y.toString, x))
    )

  // Ex 3.18 Write map, modify each element in a list while maintaining the structure of a list
  def map[A, B](as: List[A])(f: A => B): List[B] =
    reverse(
      foldLeft(as, List[B]())((x, y) => Cons(f(y), x))
    )

  // Ex 3.19 Write filter, a fn that removes elements from a list unless they satisfy a given predicate
  // Use it to remove all old numbers from a List[Int]
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    reverse(
      foldLeft(as, List[A]())((x, y) => if (f(y)) Cons(y, x) else x)
    )

  def filterOutOddNumbers(as: List[Int]): List[Int] =
    filter(as)(x => x % 2 == 0)

  // Ex 3.20 Write flatMap
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, List[B]())((x, y) => prependAll(f(y))(x))

  // Ex 3.21 Write filter in terms of flatMap
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else List())

  // Ex 3.22 Write a fn that accepts two lists and constructs a new list by adding corresponding elements
  @annotation.tailrec
  def sumTwoLists(
      l1: List[Int],
      l2: List[Int],
      acc: List[Int] = List()
  ): List[Int] = l1 match {
    case Nil => reverse(acc)
    case Cons(x, xs) =>
      l2 match {
        case Nil         => reverse(acc)
        case Cons(y, ys) => sumTwoLists(xs, ys, Cons(x + y, acc))
      }
  }

  @annotation.tailrec
  // Ex 3.23 Generalize the fn you wrote to zipWith
  def zipWith[A, B, C](
      l1: List[A],
      l2: List[B],
      f: (A, B) => C,
      acc: List[C] = List()
  ): List[C] = l1 match {
    case Nil => reverse(acc)
    case Cons(x, xs) =>
      l2 match {
        case Nil         => reverse(acc)
        case Cons(y, ys) => zipWith(xs, ys, f, Cons(f(x, y), acc))
      }
  }

}

trait TreeSpec {
  // Ex 3.25 Write a function that counts the number of nodes (leaves and branches) in a tree
  def size[A](tree: Tree[A]): Int

  // Ex 3.26 Write a function that returns the max element in the tree
  def maximum(tree: Tree[Int]): Int

  // Ex 3.27 Write a function that returns the path length from the root to a leaf
  def depth[A](tree: Tree[A]): Int

  // Ex 3.28 Write a function that modifies each element of the tree given a function
  // Note: Writing a tail-recursive map function for trees is quite tree-cky,
  // see https://stackoverflow.com/questions/55042834/how-to-make-tree-mapping-tail-recursive
  //def map[A, B](tree: Tree[A])(fn: A => B): Tree[B]
}

object NonTailRecursiveTree extends TreeSpec {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) =>
        1 + size(left) + size(right)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(i) => i
      case Branch(left, right) =>
        maximum(left).max(maximum(right))
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_)             => 0
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }
  }

  def map[A, B](tree: Tree[A])(fn: A => B): Tree[B] = {
    tree match {
      case Leaf(v)             => Leaf(fn(v))
      case Branch(left, right) => Branch(map(left)(fn), map(right)(fn))
    }
  }
}

object TailRecursiveTree extends TreeSpec {
  def size[A](tree: Tree[A]): Int = {
    @annotation.tailrec
    def recur(trees: List[Tree[A]], acc: Int = 0): Int = trees match {
      case Nil => acc
      case Cons(tree, trees) =>
        tree match {
          case Leaf(_) => recur(trees, acc + 1)
          case Branch(left, right) =>
            recur(Cons(right, Cons(left, trees)), acc + 1)
        }
    }
    recur(List(tree))
  }

  def maximum(tree: Tree[Int]): Int = {
    @annotation.tailrec
    def recur(trees: List[Tree[Int]], acc: Int = Int.MinValue): Int =
      trees match {
        case Nil => acc
        case Cons(tree, trees) =>
          tree match {
            case Leaf(v) => recur(trees, v.max(acc))
            case Branch(left, right) =>
              recur(Cons(right, Cons(left, trees)), acc)
          }
      }
    recur(List(tree))
  }

  def depth[A](tree: Tree[A]): Int = {
    @annotation.tailrec
    def recur(
        treesAndLengths: List[(Tree[A], Int)],
        maxLengthSeen: Int = 0
    ): Int = treesAndLengths match {
      case Nil => maxLengthSeen
      case Cons(treeAndLength, remainder) =>
        treeAndLength match {
          case (Leaf(_), length) => recur(remainder, maxLengthSeen.max(length))
          case (Branch(left, right), length) =>
            recur(
              Cons(
                (right, length + 1),
                Cons(
                  (left, length + 1),
                  remainder
                )
              ),
              maxLengthSeen
            )
        }
    }
    recur(List((tree, 0)))
  }

  /*
  def map[A, B](tree: Tree[A])(fn: A => B): Tree[B] = {
    // A stack frame for the tail-recursive Tree.map implementation
    case class Frame[X, Y](done: Tree[Y], todos: List[Tree[X]])
    def step(stack: List[Frame[A, B]]): Tree[B] = stack match {
      case Cons(Frame(done, Nil), remainder) => {
        remainder match {
          case Nil =>
            done match {
              case l @ Leaf(_)         => l
              case Branch(left, right) => Branch(right, left)
            }
          case Cons(Frame(done2, todo2), more) =>
            step(Cons(Frame(List.prependAll(done2)(done), todo2), more))
        }
      }
      case Cons(Frame(done, Cons(head, tail)), remainder) =>
        head match {
          // recursion base case
          case Leaf(v) =>
            step(Cons(Frame(Cons(Leaf(fn(v)), done), tail), remainder))
          case Branch(left, right) =>
            step(
              Cons(
                Frame(Nil, List(left, right)),
                Cons(Frame(done, tail), remainder)
              )
            )
        }
    }

    tree match {
      case Leaf(v) => Leaf(fn(v))
      case Branch(left, right) =>
        step(List(Frame(Nil, List(left, right))))
    }
  } */
}
