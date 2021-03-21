package chapter_2

object Exercise2_2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def rec(as: Array[A], ordered: (A, A) => Boolean, position: Int): Boolean = {
        if (as.isEmpty) true
        else if (as.length == 1) true
        else if (position + 1 == as.length) true
        else if (!ordered(as(position), as(position + 1))) false
        else rec(as, ordered, position + 1)
    }
    rec(as, ordered, 0)
  }

}