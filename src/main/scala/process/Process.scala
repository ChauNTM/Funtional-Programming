package process

import monad.Monad
import process.Process.{count1, lift}

sealed trait Process[I, O] {

  def apply(s: LazyList[I]): LazyList[O] =  this match {
    case Halt() => LazyList()
    case Await(recv) => s match {
      case h #::t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(head, tail) => head #:: tail(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this) // restart the process if it halts its own
      case Await(recv) => Await {
        case Some(i) => go(recv(Some(i)))
        case None => recv(None)
      }
      case Emit(head, tail) => Emit(head, go(tail))
    }
    go(this)
  }

  // given two Process f and g, feed output of f to the input of g,
  // p2 is f, this is g
  def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
    case Halt() => Halt()
    case Emit(head, tail) => Emit(head, this |> tail)
    case Await(f) => this match {
      case Halt() => Halt[I, O]() |> f(None)
      case Emit(head, tail) => tail |> f(Some(head))
      case Await(g) => Await((i: Option[I]) => g(i) |> p2)
    }
  }

  /**
   *  Given 2 Processes x and y, x ++ y run x to completion then run y on input remains after x halted
   * */
  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(head, tail) => Emit(head, tail ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(head, tail) => f(head) ++ tail.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  def zip[O2](p: Process[I,O2]): Process[I,(O,O2)] = Process.zip(this, p)

  def zipWithIndex: Process[I, (O, Int)] = this.zip(count1)
}

case class Halt[I, O]() extends Process[I, O]

case class Emit[I, O] (head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

object Process {
  // convert any function f: I => O to a Process[I, O]
  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I,O] = Emit[I, O](head, tail)

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => emit(i)
      case _ => Halt()
    }.repeat

  /**
   * A helper function to await an element or fall back to another process
   * if there is no input.
   */
  def await[I, O](f: I => Process[I, O],
                  fallback: Process[I, O] = Halt[I, O]()): Process[I, O] = Await {
    case Some(i) => f(i)
    case None => fallback
  }

  def take[I](n: Int): Process[I, I] =
    if (n <= 0) Halt()
    else await(i => emit(i, take(n-1)))

  def takeWhile[I](f: I => Boolean): Process[I, I] =
    await(i => if (f(i)) emit(i, takeWhile(f)) else Halt())

  def drop[I](n: Int): Process[I, I] = {
    if (n <= 0) id
    else await(_ => drop(n - 1))
  }

  /**
   * The identity `Process`, just repeatedly echos its input
   */
  def id[I]: Process[I, I] = lift(identity)

  def dropWhile[I](f: I => Boolean): Process[I, I] =
    await(i => if (f(i)) dropWhile(f) else emit(i, id))

  def count[I]: Process[I, Int] = {
    def go(n: Int): Process[I, Int] =
      await((i: I) => emit(n+1, go(n+1)))
    go(0)
  }

  // common state pattern
  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    await((i: I) => f(i, z) match {
      case (o, s2) => emit(o, loop(s2)(f))
    })

  def count1[I]: Process[I, Int] = loop(0)((_, s) => (s + 1, s + 1))

  def sum: Process[Int, Int] = loop(0)((i, s) => (i + s, i + s))

  def zip[I, O, O2](p1: Process[I, O], p2: Process[I, O2]): Process[I, (O, O2)] = {
    (p1, p2) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(h, t), Emit(h2, t2)) => Emit((h, h2), zip(t, t2))
      case (Await(recv1), _) => Await((i: Option[I]) => zip(recv1(i), feed(i)(p2)))
      case (_, Await(recv2)) => Await((i: Option[I]) => zip(feed(i)(p1), recv2(i)))

    }
  }

  def feed[I,O](i: Option[I])(p: Process[I,O]): Process[I, O] = p match {
    case Halt() => p
    case Emit(h, t) => Emit(h, feed(i)(t))
    case Await(recv) => recv(i)
  }

  def exists[I](f: I => Boolean): Process[I, Boolean] = Process.lift(f) |> any

  def any: Process[Boolean, Boolean] = Process.loop(false)((b: Boolean, s) => (s || b, s || b))

}


