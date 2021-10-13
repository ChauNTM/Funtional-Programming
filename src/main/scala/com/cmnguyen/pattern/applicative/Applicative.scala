package com.cmnguyen.pattern.applicative

import com.cmnguyen.pattern.Functor

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

}

object Applicative {
  val streamApplicative = new Applicative[LazyList] {
    override def map2[A, B, C](a: LazyList[A], b: LazyList[B])(f: (A, B) => C): LazyList[C] =
      a zip b map f.tupled

    override def unit[A](a: => A): LazyList[A] = LazyList.continually(a)

  }
}




