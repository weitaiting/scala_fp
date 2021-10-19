package chapter_6

import scala.collection.mutable.ListBuffer

trait RNG {
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

// Exercise 6.10: Generalize the functions unit, map, map2, flatMap and sequence.
// Add them as methods on the State case class where possible. Otherwise, put them in a State companion Object.
case class State[S, +A](run: S => (A, S)) {
  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = rb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      g(a).run(s2)
    })

  // Exercise 6.9: Reimplement map and map2 in terms of flatMap. The fact that this is possible is what we're referring to when we say that flatMap is more powerful than map and map2
  def mapViaFlatMap[B](f: A => B): State[S, B] =
    flatMap(a => State(s => (f(a), s)))

  def map2ViaFlatMap[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    State.both(this, rb).flatMap(tup => State(s => (f(tup._1, tup._2), s)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def both[S, A, B](s1: State[S, A], s2: State[S, B]): State[S, (A, B)] =
    s1.map2(s2)((_, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    @annotation.tailrec
    def recur(fs: List[State[S, A]], s: S, acc: ListBuffer[A]): (List[A], S) = {
      if (fs.isEmpty) {
        (acc.toList, s)
      } else {
        val (nextA, nextS) = fs.head.run(s)
        recur(fs.tail, nextS, acc :+ nextA)
      }
    }
    State(s => recur(fs, s, ListBuffer.empty))
  }
}

class Exercise6 {
  type Rand[A] = State[RNG, A]

  // Exercise 6.1: Write a fn that uses RNG.nextInt to generate a random integer between 0 and Int.MaxValue (inclusive)
  // Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn't have a non-negative counterpart
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomN, nextRNG) = rng.nextInt
    ((randomN % (Int.MaxValue + 1)).abs, nextRNG)
  }

  // Exercise 6.2: Write a function to generate a Double between 0 and 1, not including 1.
  def double(rng: RNG): (Double, RNG) = {
    val (randomN, nextRNG) = nonNegativeInt(rng)
    val d = randomN.toDouble / Int.MaxValue
    (d % 1, nextRNG)
  }

  // Exercise 6.3: Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
  // (Double, Double, Double) 3-tuple. You should reuse functions you've already written
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    val (nextDouble, nextRNG2) = double(nextRNG)
    ((nextInt, nextDouble), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nextDouble, nextRNG) = double(rng)
    val (nextInt, nextRNG2) = nextRNG.nextInt
    ((nextDouble, nextInt), nextRNG2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (nextDouble, nextRNG) = double(rng)
    val (nextDouble2, nextRNG2) = double(nextRNG)
    val (nextDouble3, nextRNG3) = double(nextRNG2)
    ((nextDouble, nextDouble2, nextDouble3), nextRNG3)
  }

  // Exercise 6.4: Write a function to generate a list of random integers
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def recur(
        count: Int,
        rng: RNG,
        acc: ListBuffer[Int] = ListBuffer.empty
    ): (List[Int], RNG) = {
      if (count == 0) {
        (acc.toList, rng)
      } else {
        val (nextInt, nextRNG) = rng.nextInt
        recur(count - 1, nextRNG, acc :+ nextInt)
      }
    }
    recur(count, rng)
  }

  val int: Rand[Int] = State(s => s.nextInt)

  def unit[A](a: A): Rand[A] = State(rng => (a, rng))

  def map[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    State(nonNegativeInt).map(i => i - i % 2)

  // Exercise 6.5: Use map to reimplement double in a more elegant way
  def double2: Rand[Double] = {
    State(nonNegativeInt).map(i => (i.toDouble / Int.MaxValue) % 1)
  }

  // Exercise 6.6: Write the implementation of map2 based on the following signature. This function takes 2 actions,
  // ra and rb, and a function f for combining their results, and returns a new action that combines them.
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    State(rng => {
      val (a, rng2) = ra.run(rng)
      val (b, rng3) = rb.run(rng2)
      (f(a, b), rng3)
    })

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, State(s => double(s)))

  val randDoubleInt: Rand[(Double, Int)] =
    both(State(s => double(s)), int)

  // Exercise 6.7 If you can combine two RNG transitions, you should be able to combine a whole list of them
  // Implement sequence for combining a List of transitions into a single transition
  // Use it to reimplement the ints function you wrote before. For the latter, you can use the standard library function
  // List.fill(n)(x) to make a list with x repeated n times
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @annotation.tailrec
    def recur(
        fs: List[Rand[A]],
        rng: RNG,
        acc: ListBuffer[A] = ListBuffer.empty
    ): (List[A], RNG) = {
      if (fs.isEmpty) {
        (acc.toList, rng)
      } else {
        val (nextA, nextRNG) = fs.head.run(rng)
        recur(fs.tail, nextRNG, acc :+ nextA)
      }
    }
    State(rng => recur(fs, rng))
  }

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = State(rng => {
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) {
      (mod, rng2)
    } else {
      // The else branch deals with the case where Int.MaxValue is not exactly divisible by n. If this is true, then
      // the numbers that are less than the remainder of that division will come up more frequently. So when
      // nonNegativeInt generates numbers higher than the largest multiple of n that fits in a 32-bit integer, we
      // should retry the generator and hope to get a smaller number.
      nonNegativeLessThan(n).run(rng2)
    }
  })

  // Exercise 6.8: implement flatMap, and then use it to implement nonNegativeLessThan
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State(rng => {
    val (a, rng2) = f.run(rng)
    g(a).run(rng2)
  })

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    State(nonNegativeInt).flatMap(i =>
      State(s => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          (mod, s)
        else nonNegativeLessThan2(n).run(s)
      })
    )
  }

}

// Exercise 6.11: To gain experience with the use of State, implement a FSA that models a simple candy
// dispenser. The machine has two types of input: you can insert a coin, or you can turn the knob to
// dispense candy. It can be in one of two states: locked or unlocked. It also tracks how many candies
// are left and how many coins it contains.
// The rules of the machine are as follows:
// - Inserting a coin into a locked machine will cause it to unlock if there's any candy left
// - Turning the knob on an unlocked machine will cause it to dispense candy and become locked
// - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing
// - A machine that's out of candy ignores all inputs
class CandyMachine {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def outOfCandy: Boolean = candies == 0
    def insertCoin: Machine =
      if (outOfCandy) this else this.copy(locked = false, coins = coins + 1)
    def turnKnob: Machine = if (outOfCandy) this
    else
      this.copy(locked = true, candies = if (locked) candies else candies - 1)
  }

  val insertCoin: State[Machine, Unit] = State(m => ((), m.insertCoin))
  val turnKnob: State[Machine, Unit] = State(m => ((), m.turnKnob))

  def processInput(input: Input): Option[State[Machine, Unit]] = input match {
    case Coin => Some(insertCoin)
    case Turn => Some(turnKnob)
    case _    => None
  }

  // The method simulateMachine should operate the machine based on the list of inputs and return the
  // number of coins and candies left in the machine at the end.
  // e.g. input Machine has 10 coins and 5 candies, and a total of 4 candies are successfully bought
  // output should be (14, 1).
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State
      .sequence(inputs.flatMap(processInput))
      .get
      .map(machine => (machine.coins, machine.candies))
  }
}
