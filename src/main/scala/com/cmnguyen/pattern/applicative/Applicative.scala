package com.cmnguyen.pattern.applicative

import com.cmnguyen.pattern.monad.Functor
import com.cmnguyen.pattern.monoid.Monoid

trait Applicative[F[_]] extends Functor[F] {

  // primitive combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinator
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, b) => map2(f(a), b)(_::_))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((a, fa) => map2(a, fa)(_::_))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        self.map2(fa._1, fb._1)(f(_, _)) -> G.map2(fa._2, fb._2)(f(_, _))

      override def unit[A](a: => A): (F[A], G[A]) = self.unit(a) -> G.unit(a)
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))

      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]()))((kfv, fm) => map2(kfv._2, fm)((v, m) => m + (kfv._1 -> v)))

}

object Applicative {

  val streamApplicative = new Applicative[LazyList] {
    override def map2[A, B, C](a: LazyList[A], b: LazyList[B])(f: (A, B) => C): LazyList[C] =
      a zip b map f.tupled

    override def unit[A](a: => A): LazyList[A] = LazyList.continually(a)
  }

  val optionApplicative = new Applicative[Option] {
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  val listApplicative = new Applicative[List] {
    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
      fa zip fb map f.tupled

    override def unit[A](a: => A): List[A] = List(a)
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({ type f[x] = Const[M, x] })#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      override def map2[A, B, C](m1: Const[M, A], m2: Const[M, B])(f: (A, B) => C): Const[M, C] = M.op(m1, m2)

      override def unit[A](a: => A): Const[M, A] = M.zero
    }
}



