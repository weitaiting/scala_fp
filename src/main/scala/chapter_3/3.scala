package chapter_3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Ex 3.2: Returns a list with the first element removed
  def tail[A](items: List[A]): List[A] = items match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // Ex 3.3: Replace the first element of a list with a different value
  def setHead[A](items: List[A], sub: A): List[A] = items match {
    case Nil => Nil
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
    case Nil => acc
    case Cons(x, xs) => reverse(xs, Cons(x, acc))
  }

  // Ex 3.6: Remove the last element of a List
  // a non-tail-recursive solution
  def init2[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init2(xs))
  }

  // a tail-recursive solution
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def tailRec[A](l: List[A], acc: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => reverse(acc)
      case Cons(x, xs) => tailRec(xs, Cons(x, acc))
    }
    tailRec(l, Nil)
  }

  // NOT tail-recursive, and hence not stack-safe
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
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
    foldRight(as, 0)((x, y) => y + 1)

  // Ex 3.10 Write a tail-recursive foldLeft
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  
  // Ex 3.11 Write tail-recursive sum, product, and length using foldLeft
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)
  
  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)((x, y) => x * y)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((x, y) => x + 1)

  // Ex 3.12 Write a reverse implemented with foldLeft
  def reverse2[A](ns: List[A]) =
    foldLeft(ns, List[A]())((x, y) => Cons(y, x))
  
      


}