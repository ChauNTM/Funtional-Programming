package com.cmnguyen.pattern.monoid

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(l1: List[A], l2: List[A]): List[A] = l1 ++ l2
    val zero = List.empty
  }

  val intAddition = new Monoid[Int] {
    def op(n1: Int, n2: Int): Int = n1+n2
    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(o1: Boolean, o2: Boolean) = o1 || o2
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(o1: Boolean, o2: Boolean) = o1 && o2
    def zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]) = o1 orElse o2
    def zero = None
  }

  def edoMonoid[A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) : A => A = f andThen  g
    def zero = (a: A) => a
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  def foldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.length == 1) return f(v(0))
    val (left, right) = v.splitAt(v.length/2)
    m.op(foldMap(left, m)(f), foldMap(right, m)(f))
  }

  def mapMergeMonoid[K, V] (v: Monoid[V]): Monoid[Map[K, V]] = {
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero) { (map, key) =>
          map.updated(
            key,
            v.op(
              a1.getOrElse(key, v.zero),
              a2.getOrElse(key, v.zero)
            )
          )
        }

      override def zero: Map[K, V] = Map[K, V]()
    }
  }
  val x: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = A.op(a1._1, a2._1) -> B.op(a1._2, a2._2)
      override def zero: (A, B) = A.zero -> B.zero
    }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = a => b.op(a1(a), a2(a))
    override def zero: A => B = _ => b.zero
  }

  def test(): Unit = {
    val m = productMonoid(intAddition, intAddition)
    val p = ListFoldable.foldMap(List(1,2,3,4))(a => 1 -> a)(m)
  }

  val wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(x), Stub(y)) => Stub(x+y)
      case (Stub(x), Part(l, w, r)) => Part(l + x, w, r)
      case (Part(l, w, r), Stub(y)) => Part(l, w, r + y)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
    override def zero: WC = Stub("")
  }

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }


}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

  override def toList[A](fa: List[A]): List[A] = fa
}
